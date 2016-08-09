% inspired from 'https://github.com/robertoaloi/eunit_terms/blob/master/eunit_terms.erl'
-module(eunit_jsonreport).

-behaviour(eunit_listener).

-include_lib("eunit/include/eunit.hrl").

-export([start/0, start/1]).

-export([init/1,
         handle_begin/3,
         handle_end/3,
         handle_cancel/3,
         terminate/2]).

-export_record_info([testsuite, testcase]).

%% ============================================================================
%% TYPES
%% ============================================================================
-type(chars() :: [char() | any()]). % chars()

%% ============================================================================
%% RECORDS
%% ============================================================================
-record(testcase, {
          displayname :: chars(),
          description :: chars(),
          module :: chars(),
          function :: char(),
          arity :: integer(),
          line :: integer(),
          result :: ok | {failed, tuple()} | {aborted, tuple()} | {skipped, tuple()},
          time :: integer(),
          output :: binary()
         }).
-record(testsuite, {
          name = <<>> :: binary(),
          time = 0 :: integer(),
          output = <<>> :: binary(),
          succeeded = 0 :: integer(),
          failed = 0 :: integer(),
          aborted = 0 :: integer(),
          skipped = 0 :: integer(),
          testcases = [] :: [#testcase{}]
         }).
-record(state, {
          verbose = false,
          dir = ".",
          testsuite = #testsuite{}
         }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    Dir = proplists:get_value(dir, Options, "."),
    SuiteName = proplists:get_value(testsuitename, Options, "testsuite"),
    State = #state{verbose = proplists:get_bool(verbose, Options),
                dir = Dir,
                testsuite = #testsuite{name=SuiteName}},
    receive
        {start, _Reference} ->
            State
    end.

-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).

%because lists:join is recently added to erlang (doesn't exists in 6.4), I pick up the code source
lists_join(_Sep, []) -> [];
lists_join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].


tuples_to_json(L) ->
    "{" ++ 
        lists_join(",", lists:map(fun(X)-> tuple_to_json(X) end, L)) ++
    "}".

tuple_to_json({Name, Value}) ->
    "\""++io_lib:print(Name)++"\":" ++ to_json(Value);

tuple_to_json(X) ->
    "\"xxx" ++ io_lib:print(X) ++ "xx\"".

stack_to_json({M, F, A, [{file, FileName}, {line, Line}]}) ->
    tuples_to_json([{module, M}, {function, F}, {arity, A}, {file, FileName}, {line, Line}]).

record_to_json(R) ->
    ["{"] ++ lists_join(",", lists:map(fun(X)-> tuple_to_json(X) end, R)) ++ ["}"].

location_to_json(L) ->
    NewL = lists:map(
        fun(X) ->            
            case X of
                {value, V} -> {value, "\"" ++ format_string(io_lib:print(V)) ++ "\""};
                {expected, E} -> {expected, "\"" ++ format_string(io_lib:print(E))  ++"\"" };
                Y -> Y
            end 
        end, L),
    tuples_to_json(NewL).

to_json(TS) when is_record(TS, testsuite) ->
    Json = record_to_json(?record_to_tuplelist(testsuite, TS)),
    %io:format(Json),
    Json;
to_json([H|_T]) when is_record(H, testcase) ->
    "[" ++ to_json(H) ++ 
    "]";
to_json(TC) when is_record(TC, testcase) ->
    L = record_to_json(?record_to_tuplelist(testcase, TC)),
    L;

to_json({aborted, {error, Err, StackList}}) ->
    "{\"aborted\": {\"error\" : \"" ++ io_lib:print(Err) ++ "\"," ++
    "\"stacktrace\" : " ++ ["["] ++ lists_join(",", lists:map(fun(X)-> stack_to_json(X) end, StackList)) ++ ["]"]
    ++"}}"; 

to_json({error, {AssertName, AssertStack}, StackList}) ->
    "{\"assertion\" :\""++ atom_to_list(AssertName) ++"\", \"location\" :" ++ location_to_json(AssertStack) ++ "," ++
      "\"stacktrace\" : " ++ ["["] ++ lists_join(",", lists:map(fun(X)-> stack_to_json(X) end, StackList)) ++ ["]"]
    ++"}";


to_json(V) when is_bitstring(V) ->
    io_lib:print(format_string(binary_to_list(V)));

to_json(V) when is_binary(V)->
    %"\"binary\"";
    binary_to_json(V); 

to_json(V) when is_atom(V) ->
    io_lib:print(atom_to_list(V));
to_json(V) when is_tuple(V) ->
    %"\"tuple\"";
     ["{"] ++ tuple_to_json(V) ++ ["}"];     

to_json(V) ->
    case io_lib:printable_list(V) of
    true -> io_lib:print(format_string(V));
    false -> 
        case is_binary(V) of
        true -> "\"" ++ io_lib:print(binary_to_list(V)) ++ "\"";
        %false - to_json(V)
        false -> "\"" ++ format_string(io_lib:print(V)) ++ "\""
        %false -> "\"printable_binary\""
        end
    end.

binary_to_json(V) ->
    NewV=binary_to_term(V),
    %"\"" ++ format_string(io_lib:print(V)) ++ "\"".
    to_json(NewV).

format_string(S) ->
    S1 = re:replace(S, "\n", "\\\\n", [global, {return, list}]),
    re:replace(S1, "\"", "'", [global, {return, list}]).

terminate({ok, _Data}, #state{testsuite = TS} = State) ->
    Dir = State#state.dir,
    JSon = to_json(TS),
    file:write_file(filename:join([Dir, "testsuite_results.json"]), JSon),
    ok;
terminate({error, Reason}, _St) ->
    io:fwrite("Internal error: ~P.\n", [Reason, 25]),
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

handle_begin(group, Data, State) ->
    NewId = proplists:get_value(id, Data),
    case NewId of
	[] ->
	    State;
	[_GroupId] ->
	    Desc = proplists:get_value(desc, Data),
	    TestSuite = State#state.testsuite,
	    NewTestSuite = TestSuite#testsuite{name = Desc},
	    State#state{testsuite=NewTestSuite};
	%% Surefire format is not hierarchic: Ignore subgroups:
	_ ->
	    State
    end;
handle_begin(test, _Data, State) ->
    State.
handle_end(group, Data, St) ->
    %% Retrieve existing test suite:
    case proplists:get_value(id, Data) of
	[] ->
	    St;
	[_GroupId|_] ->
	    TestSuite = St#state.testsuite,

	    %% Update TestSuite data:
	    Time = proplists:get_value(time, Data),
	    Output = proplists:get_value(output, Data),
	    NewTestSuite = TestSuite#testsuite{ time = Time, output = Output },
	    St#state{testsuite=NewTestSuite}
    end;
handle_end(test, Data, State) ->
    %% Retrieve existing test suite:
    TestSuite = State#state.testsuite,
    %io:format(io_lib:print(Data)),
    %% Create test case:
    {Module, Function, Arity} =proplists:get_value(source, Data),
    Line = proplists:get_value(line, Data), 
    Name = lists:flatten(io_lib:format("~p:~p/~p(~p)",[Module, Function, Arity, Line])),
    Desc = format_desc(proplists:get_value(desc, Data)),
    Result = proplists:get_value(status, Data),
    Time = proplists:get_value(time, Data),
    Output = proplists:get_value(output, Data),
    TestCase = #testcase{displayname = Name,
        module=Module, function=Function, arity=Arity, line=Line, 
        description = Desc,
	    time = Time,output = Output},
    NewTestSuite = add_testcase_to_testsuite(Result, TestCase, TestSuite),
    State#state{testsuite=NewTestSuite}.

%% Cancel group does not give information on the individual cancelled test case
%% We ignore this event
handle_cancel(group, _Data, State) ->
    State;
handle_cancel(test, Data, State) ->
    %% Retrieve existing test suite:
    TestSuite = State#state.testsuite,

    %% Create test case:
    Name = format_name(proplists:get_value(source, Data),
		       proplists:get_value(line, Data)),
    Desc = format_desc(proplists:get_value(desc, Data)),
    Reason = proplists:get_value(reason, Data),
    TestCase = #testcase{
      displayname = Name, description = Desc,
      result = {skipped, Reason}, time = 0,
      output = <<>>},
    NewTestSuite = TestSuite#testsuite{
		     skipped = TestSuite#testsuite.skipped+1,
		     testcases=[TestCase|TestSuite#testsuite.testcases] },
    State#state{testsuite=NewTestSuite}.

format_name({Module, Function, Arity}, Line) ->
    {Module, Function, Arity, Line}.

format_desc(undefined) ->
    "";
format_desc(Desc) when is_binary(Desc) ->
    binary_to_list(Desc);
format_desc(Desc) when is_list(Desc) ->
    Desc.

%% Add testcase to testsuite depending on the result of the test.
add_testcase_to_testsuite(ok, TestCaseTmp, TestSuite) ->
    TestCase = TestCaseTmp#testcase{ result = ok },
    TestSuite#testsuite{
      succeeded = TestSuite#testsuite.succeeded+1,
      testcases=[TestCase|TestSuite#testsuite.testcases] };
add_testcase_to_testsuite({error, Exception}, TestCaseTmp, TestSuite) ->
    case Exception of
	{error,{AssertionException,_},_} when
              AssertionException == assertion_failed;
              AssertionException == assertMatch_failed;
              AssertionException == assertEqual_failed;
              AssertionException == assertException_failed;
              AssertionException == assertCmd_failed;
              AssertionException == assertCmdOutput_failed
              ->
	    TestCase = TestCaseTmp#testcase{ result = {failed, Exception} },
	    TestSuite#testsuite{
	      failed = TestSuite#testsuite.failed+1,
	      testcases = [TestCase|TestSuite#testsuite.testcases] };
	_ ->
	    TestCase = TestCaseTmp#testcase{ result = {aborted, Exception} },
	    TestSuite#testsuite{
	      aborted = TestSuite#testsuite.aborted+1,
	      testcases = [TestCase|TestSuite#testsuite.testcases] }
    end.