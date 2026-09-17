-module(gen_lsp_server).
-behavior(gen_server).

%inspired from https://github.com/kevinlynx/erlang-tcpserver/blob/master/test/test.erl
%http://20bits.com/article/erlang-a-generalized-tcp-server

% à regarder
% http://learnyousomeerlang.com/buckets-of-sockets


%% API
-export([start_link/1, start_link/2]).
-export([lsp_log/2, lsp_log/3, send_to_client/3]).
-export([next_request_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
%state
-record(state, {socket, content_length, contents}).

% because stacktrace is deprecated from OTP 21
-ifdef(OTP_RELEASE).
safeApply(Function, Socket, ArgsMap) ->
    try apply(lsp_handlers, Function, [Socket, ArgsMap]) of
        {error, ErrorResult } -> {error, ErrorResult };
        Result -> {ok, Result}
    catch
        throw:Reason when is_binary(Reason) ->
            lsp_log("LSP handler returned '~p'", [Reason]),
            {handler_error, Reason};                    
        throw:Reason when is_list(Reason) ->
            lsp_log("LSP handler returned '~p'", [Reason]),
            {handler_error, list_to_binary(Reason)};
        Error:Exception:StackTrace ->
            lsp_log:error(<<"LSP">>, "LSP handler error ~p:~p while executing lsp_handlers:~p(_, ~p), stacktrace:~p", 
                    [Error, Exception,Function,ArgsMap, StackTrace]),
            {handler_error, <<"Handler error">>}
    end. 

-else.
safeApply(Function, Socket, ArgsMap) ->
    try apply(lsp_handlers, Function, [Socket, ArgsMap]) of
        {error, ErrorResult } -> {error, ErrorResult };
        Result -> {ok, Result}
    catch
        throw:Reason when is_binary(Reason) ->
            lsp_log("LSP handler returned '~p'", [Reason]),
            {handler_error, Reason};                    
        throw:Reason when is_list(Reason) ->
            lsp_log("LSP handler returned '~p'", [Reason]),
            {handler_error, list_to_binary(Reason)};
        Error:Exception ->
            lsp_log:error(<<"LSP">>,"LSP handler error ~p:~p while executing lsp_handlers:~p(_, ~p), stacktrace:~p", 
                    [Error, Exception,Function,ArgsMap, erlang:get_stacktrace()]),
            {handler_error, <<"Handler error">>}
    end.

-endif.


start_link(VsCodePort) ->
    start_link(VsCodePort, undefined).

start_link(VsCodePort, Socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [VsCodePort, Socket, self()],[]).

init([_VsCodePort, Socket, _Parent]) ->
    {ok, #state{socket = Socket, contents = <<"">>}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, Contents}, State) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, handle_tcp_data(Socket, Contents, State)};
handle_info(timeout, #state{socket = Socket} = State) ->
    {ok, _} = gen_tcp:accept(Socket), 
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(_Data, State) ->
    {noreply, State}.

lsp_log(Msg, Args) ->
    gen_lsp_config_server:verbose() andalso lsp_log:info(<<"LSP">>, Msg, Args).

lsp_log(Method, Msg, Args) ->
    % Method can be excluded from verbose logging by adding it to verboseExcludeFilter in the config
    gen_lsp_config_server:verbose() andalso gen_lsp_config_server:verbose_is_include(Method) andalso lsp_log:info(Method, Msg, Args).

%% @doc JSON-RPC ids of server-initiated requests must be unique per in-flight
%% request. Base carries provenance (e.g. <<"configuration#initialized">>) and
%% survives into the log line; base_request_id/1 strips the sequence back off
%% when the client's response is routed to a handler.
next_request_id(Base) ->
    Seq = erlang:unique_integer([positive, monotonic]),
    <<Base/binary, "#", (integer_to_binary(Seq))/binary>>.

base_request_id(Id) when is_binary(Id) ->
    hd(binary:split(Id, <<"#">>));
base_request_id(Id) ->
    Id.

remove_text_for_logging(#{params := #{contentChanges := ChangesList} = Params} = Input) ->
    Input#{params := Params#{contentChanges := lists:map(fun 
        (#{text := <<Text/binary>>} = Change) when byte_size(Text) > 20 ->
            Cut = binary:part(Text, 0, 20),
            Change#{text := <<Cut/binary, " ...">>};
        (Change) ->
            Change
    end, ChangesList)}};
remove_text_for_logging(#{params := #{textDocument := #{text := Text} = TextDocument} = Params} = Input) when byte_size(Text) > 20 ->
    Cut = binary:part(Text, 0, 20),
    Input#{params := Params#{textDocument := TextDocument#{text := <<Cut/binary, " ...">>}}};
remove_text_for_logging(Input) ->
    Input.

do_contents(Socket, #{method := Method} = Input) ->
    %% Always-on compact trace (gated only by erlang.verbose, not by the
    %% per-method exclude filter) so the sequence/timing of every message
    %% stays visible even with the noisy methods excluded below.
    lsp_log("LSP received ~s id=~p", [Method, maps:get(id, Input, undefined)]),
    lsp_log(Method, "LSP received detail ~p", [remove_text_for_logging(Input)]),
    case call_handler(Socket, Method, maps:get(params, Input, undefined)) of
        {ok, Result} ->
            send_response_with_id(Socket, Input, #{result => Result});
        {error, Result} ->
            send_response_with_id(Socket, Input, #{error => Result});
        handler_not_found ->
            lsp_log:error(<<"LSP">>, "Method not handled: ~p", [Method]),
            send_response_with_id(Socket, Input, #{error => #{code => -32001, message => <<"Method not handled">>}});
        {handler_error, Message} ->
            send_response_with_id(Socket, Input, #{error => #{code => -32001, message => Message}})
    end;

do_contents(Socket, #{id := Id} = Input) ->
    %% Client's reply to a server-initiated request (e.g. workspace/configuration,
    %% workspace/diagnostic/refresh) - Id is the atom/binary we picked when
    %% we sent that request, so this line alone lets us see when/if the
    %% client acknowledged it, without the full result body.
    lsp_log("LSP received response id=~p", [Id]),
    lsp_log(<<"lsp/response">>, "LSP received response detail ~p", [Input]),
    case call_handler(Socket, base_request_id(Id), maps:get(result, Input, undefined)) of
        {ok, _Result} ->
            ok;
        {error, _Result} ->
            ok;
        handler_not_found ->
            lsp_log:error(<<"LSP">>, "Notification not handled: ~p ~p", [Id, Input]);
        {handler_error, Message} ->
            Message
    end.

call_handler(Socket, Name, ArgsMap) ->
    case lists:keyfind(handler_name(Name), 1, lsp_handlers:module_info(exports)) of
        false ->
            handler_not_found;
        {Function, 2} ->
            %uncomment to show commands sent by vscode
            %lsp_log("LSP call_handler lsp_handlers:'~p':~p", [Function, ArgsMap]),
            safeApply(Function, Socket, ArgsMap)
    end.

handler_name(<<"$/", Name/binary>>) ->
    list_to_atom(binary_to_list(Name));
%% CHARACTERIZATION: every method name up to task 3.2 had at most one `/`
%% (e.g. `textDocument/codeAction`), so a non-global replace happened to
%% work. `textDocument/semanticTokens/full/delta` has three - without
%% `[global]` this silently produced an atom that still contained `/`
%% characters (matching no exported handler) - never caught until task 3.2
%% actually tried to route a method name with more than one `/`.
handler_name(Name) ->
    list_to_atom(binary_to_list(binary:replace(Name, <<"/">>, <<"_">>, [global]))).

send_response_with_id(Socket, #{method := Method} = Input, Response) ->
    case maps:get(id, Input, undefined) of
        undefined ->
            ok;
        Id ->
            send_to_client(Socket, Method, Response#{id => Id})
    end.

send_to_client(Socket, Method, Body) ->
    %% Always-on compact trace, see do_contents/2 comment above.
    lsp_log("LSP sends ~s id=~p", [Method, maps:get(id, Body, undefined)]),
    lsp_log(Method, "LSP sends detail ~p", [Body]),
    {ok, Json} = vscode_jsone:encode(Body),
    Header = iolist_to_binary(io_lib:fwrite("Content-Length: ~p", [byte_size(Json)])),
    gen_tcp:send(Socket, <<Header/binary, "\r\n\r\n", Json/binary>>).


handle_tcp_data(Socket, Contents, State) ->
    StateWithContents = State#state{contents = <<(State#state.contents)/binary, Contents/binary>>},
    StateWithLength = case StateWithContents#state.content_length of
        undefined ->
            HeadersEnd = binary:match(StateWithContents#state.contents, <<"\r\n\r\n">>),
            case HeadersEnd of
                nomatch ->
                    StateWithContents;
                {HeadersSeparatorStart, HeadersSeparatorLen} ->
                    {match, [_, {LengthStart, LengthLen}]} =
                        re:run(StateWithContents#state.contents, "Content-Length: *([0-9]+)"),
                    Length = binary_to_integer(binary:part(StateWithContents#state.contents, LengthStart, LengthLen)),
                    BodyStart = HeadersSeparatorStart + HeadersSeparatorLen,
                    BodyLen = byte_size(StateWithContents#state.contents) - BodyStart,
                    StateWithContents#state{
                        contents = binary:part(StateWithContents#state.contents, BodyStart, BodyLen),
                        content_length = Length
                    }
            end;
        _ ->
            StateWithContents
    end,
    case StateWithLength#state.content_length of
        undefined ->
            StateWithLength;
        ContentLength when ContentLength > byte_size(StateWithLength#state.contents) ->
            StateWithLength;
        ContentLength when ContentLength =:= byte_size(StateWithLength#state.contents) ->
            {ok, Input, _} = vscode_jsone_decode:decode(StateWithLength#state.contents, [{keys, atom}]),
            spawn(fun() -> do_contents(Socket, Input) end),
            StateWithLength#state{contents = <<"">>, content_length = undefined};
        ContentLength when ContentLength < byte_size(StateWithLength#state.contents) ->
            ShorterContents = binary:part(StateWithLength#state.contents, 0, ContentLength),
            {ok, Input, _} = vscode_jsone_decode:decode(ShorterContents, [{keys, atom}]),
            spawn(fun() -> do_contents(Socket, Input) end),
            handle_tcp_data(
                Socket,
                binary:part(StateWithLength#state.contents, ContentLength, byte_size(StateWithLength#state.contents) - ContentLength),
                StateWithLength#state{contents = <<"">>, content_length = undefined})
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
