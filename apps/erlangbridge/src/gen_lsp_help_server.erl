-module(gen_lsp_help_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_help/2]).

-define(SERVER, ?MODULE).

-record(state, {modules}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

get_help(Module, Function) -> 
    gen_server:call(?SERVER, {get_help, Module, Function}).

init(_Args) ->
    {ok, #state{modules=#{}}}.

handle_call({get_help, Module, Function}, _From, State) ->
    {Lines, Modules} = get_page_lines(Module, State#state.modules),
    Reply = case Lines of
        undefined -> undefined;
        _ -> markdown(extract_function(Function, Lines))
    end,
    {reply, Reply, State#state{modules = Modules}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

get_page_lines(Module, Modules) ->
    case Modules of
        #{Module := Lines} = _ ->
            {Lines, Modules};
        _ ->
            Result = httpc:request("http://erlang.org/doc/man/" ++ atom_to_list(Module) ++ ".html"),
            case Result of
                {ok, {_, _, Body}} ->
                    Lines = lists:map(fun (Line) ->
                        case re:run(Line, <<"^ *(.*)$">>) of
                            {match, [_, {Start, Len}]} -> binary:part(Line, Start, Len);
                            _ -> Line
                        end
                    end, binary:split(list_to_binary(Body), <<"\n">>, [global, trim])),
                    {Lines, Modules#{Module => Lines}};
                _ ->
                    {undefined, Modules}
            end
    end.

extract_function(Function, Lines) ->
    FunctionBinary = list_to_binary(atom_to_list(Function)),
    {Extracted, Completed} = lists:foldl(fun
        (Line, {undefined, false}) ->
            case binary:match(Line, <<"name=\"", FunctionBinary/binary>>) of
                nomatch -> {undefined, false};
                _ -> {Line, false}
            end;
        (<<>>, {Contents, false}) ->
            {Contents, true};
        (Line, {Contents, false}) ->
            NameEq = binary:match(Line, <<"name=\"">>),
            case NameEq of
                {Start, _} ->
                    MyNameEq = binary:match(Line, <<"name=\"", FunctionBinary/binary>>),
                    case MyNameEq of
                        {Start, _} -> {<<Contents/binary, " ", Line/binary>>, false};
                        _ -> {Contents, true}
                    end;
                _ ->
                    {<<Contents/binary, " ", Line/binary>>, false}
            end;
        (_Line, {Contents, true}) ->
            {Contents, true}
    end, {undefined, false}, Lines),
    case Completed of
        true -> Extracted;
        _ -> undefined
    end.

markdown(undefined) ->
    undefined;
markdown(Text) ->
    markdown(Text, <<>>, []).

markdown(Text, Output, TagsStack) ->
    case re:run(Text, <<"^< *(/?) *([a-zA-Z0-9]+)([^>]*)>(.*)$">>) of
        nomatch ->
            case re:run(Text, <<"^([^<]+)(.*)$">>) of
                nomatch ->
                    <<Output/binary, Text/binary>>;
                {match, [_, OutsideTag, Rest]} ->
                    OutsideTagText = binary:part(Text, OutsideTag),
                    markdown(binary:part(Text, Rest), <<Output/binary, OutsideTagText/binary>>, TagsStack)
            end;
        {match, [_, {_, EndTagSlashLen}, Tag, Attributes, Rest]} ->
            {Processed, UpdatedTagsStack} = if
                EndTagSlashLen > 0 ->
                    end_tag(TagsStack);
                true ->
                    tag(binary:part(Text, Tag), binary:part(Text, Attributes), TagsStack)
            end,
            markdown(binary:part(Text, Rest), <<Output/binary, Processed/binary>>, UpdatedTagsStack)
    end.

tag(_Tag, _Attributes, [off | TagsStackTail]) ->
    {<<>>, [off | [off | TagsStackTail]]};
tag(<<"br">>, _Attributes, TagsStack) ->
    {<<"  \n">>, TagsStack};
tag(<<"p">>, _Attributes, TagsStack) ->
    {<<>>, [<<"  \n">> | TagsStack]};
tag(<<"dt">>, _Attributes, TagsStack) ->
    {<<"  \n**">>, [<<"**  \n">> | TagsStack]};
tag(<<"strong">>, _Attributes, TagsStack) ->
    {<<" **">>, [<<"** ">> | TagsStack]};
tag(<<"h3">>, Attributes, TagsStack) ->
    case re:run(Attributes, <<"func-types-title">>) of
        nomatch ->
            {<<"\n#### ">>, [<<"\n">> | TagsStack]};
        _ ->
            {<<"  \n">>, [<<"  \n">> | TagsStack]}
    end;
tag(_Tag, Attributes, TagsStack) ->
    case re:run(Attributes, <<"bold_code">>) of
        nomatch ->
            case re:run(Attributes, <<"name=">>) of
                nomatch ->
                    case re:run(Attributes, <<"REFTYPES|func-types-title">>) of
                        nomatch ->
                            case re:run(Attributes, <<"REFBODY">>) of
                                nomatch -> {<<>>, [<<>> | TagsStack]};
                                _ -> {<<"  \n">>, [<<>> | TagsStack]}
                            end;
                        _ ->
                            {<<>>, [off | TagsStack]}
                    end;
                _ ->
                    {<<>>, [<<"  \n">> | TagsStack]}
            end;
        _ ->
            {<<" **">>, [<<"**">> | TagsStack]}
    end.

end_tag([off | TagsStackTail]) ->
    {<<>>, TagsStackTail};
end_tag([TagsStackTop | TagsStackTail]) ->
    {TagsStackTop, TagsStackTail};
end_tag(TagsStack) ->
    {<<>>, TagsStack}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
