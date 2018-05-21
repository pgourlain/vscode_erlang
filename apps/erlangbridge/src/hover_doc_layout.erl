%%
%% Inpired from 'https://github.com/erlang/otp/blob/master/lib/edoc/src/edoc_layout.erl'
%%
-module(hover_doc_layout).


-export([module/2]).

-include_lib("xmerl/include/xmerl.hrl").

%% @doc return a string
module(Element, Options) ->
    Functions = layout_module(Element, init_opts(Element,Options)),
    %return only description
    Ret = [FDesc || {_FName, FDesc} <- Functions],
    lists:flatten(join_strings(Ret, "  \n")).

init_opts(_Element, Options) ->
    Options.

layout_module({_, Xml}, Opts) ->
    layout_module(Xml,Opts);

layout_module(#xmlElement{name = module, content = Es}, Opts) ->
    % Filter is like this : {filter, [{function, load_xy, 1}]}
    FnFilter = case proplists:get_value(filter, Opts) of
    undefined -> fun (_) -> true end;
    Array -> 
        case proplists:get_value(function, Array) of
        {FName, Arity} -> fun (X) -> are_function_equal(FName, Arity, X)  end;
        _ -> fun (_) -> true end
        end
    end,
    % filter the functions according to given options parameters
    Functions = [{function_name(E, Opts), function_description(E, Opts)} ||
                    E <- get_content(functions, Es), FnFilter(E)],
    Functions.

are_function_equal(FName, Arity, E) ->
    N = list_to_atom(get_attrval(name, E)),
    A = get_attrval(arity, E),
    Result = N == FName andalso length(A) >0 andalso Arity == list_to_integer(A),
    Result.

function_description(E, _Opts) ->
    Content = E#xmlElement.content,
    Desc = get_content(description, Content),
    %io:format("description : ~p~n", [Desc]),
    FullDesc = get_text(fullDescription, Desc),
    %replace all '\n' by '  \n' (two spaces) for markdown rendering
    lists:flatten(add_spaces(FullDesc)).

add_spaces(Str) ->
    lists:reverse(add_spaces(Str, "")).
add_spaces("", Acc) ->
    Acc;
add_spaces([$\n | T], Acc) ->
    add_spaces(T, [$\n, $ ,$  | Acc ]);
add_spaces([H | T], Acc) ->
    add_spaces(T, [H | Acc]).

function_name(E, _Opts) ->
    Children = E#xmlElement.content,
    Name = get_attrval(name, E),
    lists:flatten(Name ++ "(" ++ function_args(get_content(args, Children)) ++ ")"). 

function_args(Es) ->
    Args = [get_text(argName, Arg#xmlElement.content) || Arg <- get_elem(arg, Es)],
    join_strings(Args, ",").

get_elem(Name, [#xmlElement{name = Name} = E | Es]) ->
    [E | get_elem(Name, Es)];
get_elem(Name, [_ | Es]) ->
    get_elem(Name, Es);
get_elem(_, []) ->
    [].

get_content(Name, Es) ->
    case get_elem(Name, Es) of
	[#xmlElement{content = Es1}] ->
	    Es1;
	[] -> []
    end.

get_text(Name, Es) ->
    case get_content(Name, Es) of
	[#xmlText{value = Text}] ->
	    Text;
    [] -> "";
    [#xmlElement{name=p, content= Es1}|_OtherXmlText] ->
        lists:flatten([T || T <- get_text_value(Es1 ++ _OtherXmlText)]);
    _Other ->
        error_logger:warning_msg("~p:get_text unknown xml content : ~p~n  ", [?MODULE, _Other]), 
        ""
    end.

get_text_value([#xmlText{value = Text}|T]) ->
    Text ++ get_text_value(T);
get_text_value([]) ->
    "";
get_text_value([#xmlElement{name=p, content=Es}|T]) ->
    get_text_value(Es) ++ get_text_value(T);
get_text_value([_H|T]) ->
    %%ignore _H, it's not an XmlText or 'p' element 
    get_text_value(T).


get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

join_strings([], _) ->
    [];
join_strings([String], _) ->
    String;
join_strings([String|Rest], Joiner) ->
    String ++ Joiner ++ join_strings(Rest, Joiner).