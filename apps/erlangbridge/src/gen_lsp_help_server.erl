-module(gen_lsp_help_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_help/2]).

-define(SERVER, ?MODULE).

-record(state, {modules}).

-ifdef(OTP_RELEASE).
    -record(uri_map, {host, port}).
    -if(?OTP_RELEASE >= 23).
    -include_lib("kernel/include/eep48.hrl").
    -else.
    %only to avoid complation error on OTP < 23
    -record(docs_v1, {docs}).
    %% appear in OTP 23.0
    atom_to_binary(Atom) ->
        list_to_binary(atom_to_list(Atom)).
    -endif.
-else.
%only to avoid complation error on OTP < 23
-record(docs_v1, {docs}).
%% appear in OTP 23.0
atom_to_binary(Atom) ->
    list_to_binary(atom_to_list(Atom)).

-endif.


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

get_help(Module, Function) -> 
    gen_server:call(?SERVER, {get_help, Module, Function}).

init(_Args) ->
    {ok, #state{modules=#{}}}.

handle_call({get_help, Module, Function}, _From, State) ->
    %gen_lsp_server:lsp_log("get_help for ~p:~p",[Module,Function]),
    {Reply, Modules} = case get_help_eep48(Module, Function) of
        {error, _} -> 
            {Lines, Ms} = get_page_lines(Module, State#state.modules),
            case Lines of
                undefined -> {undefined, Ms};
                _ -> { markdown(extract_function(Function, Lines)), Ms}
            end;
        Help ->
            FlattenHelp = list_to_binary(lists:flatten(Help)),
            %gen_lsp_server:lsp_log("Help:~p",[FlattenHelp]),
            {FlattenHelp, State#state.modules}
    end,
    {reply, Reply, State#state{modules = Modules}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE >= 23).
    get_help_eep48(Module, Function) ->
        case gen_lsp_config_server:eep48_help() of
            true ->
                case code:get_doc(Module) of
                    {ok, HelpModule} -> eep48_render_fun_doc(Module, Function, HelpModule);
                    _ -> {error, doc_unavailable}
                end;
            _ -> 
                gen_lsp_server:lsp_log("get_help_eep48 not enabled in config",[]),
                {error, eep48_not_enabled}
        end.
    -else.
    get_help_eep48(_Module, _Function) ->
        %gen_lsp_server:lsp_log("get_help_eep48 notsupported",[]),
        {error, eep48_not_supported}.
    -endif.
-else.
    get_help_eep48(_Module, _Function) ->
        %gen_lsp_server:lsp_log("get_help_eep48 notsupported",[]),
        {error, eep48_not_supported}.
-endif.

eep48_render_fun_doc(_Module, Function, #docs_v1{ docs = Docs } = D) ->
    FnDoc = lists:filter(fun({{function, F, _},_Anno,_Sig,_Doc,_Meta}) ->
                             F =:= Function;
                        (_) ->
                             false
                     end, Docs),
    render_function(FnDoc, D).

render_function([], _D) ->
    {error, function_missing};
render_function(FDocs, #docs_v1{ docs = Docs } = _D) ->
    Grouping =
        lists:foldl(
          fun({_Group,_Anno,_Sig,_Doc,#{ equiv := Group }} = Func,Acc) ->
                  Members = maps:get(Group, Acc, []),
                  Acc#{ Group => [Func|Members] };
             ({Group, _Anno, _Sig, _Doc, _Meta} = Func, Acc) ->
                  Members = maps:get(Group, Acc, []),
                  Acc#{ Group => [Func|Members] }
          end, #{}, lists:sort(FDocs)),
          lists:map(
      fun({{_,F,A} = Group,Members}) ->
              Signatures = lists:flatmap(fun render_signature/1,lists:reverse(Members)),
              case lists:search(fun({_,_,_,Doc,_}) ->
                                        Doc =/= #{}
                                end, Members) of
                  {value, {_,_,_,Doc,_Meta}} ->
                      render_headers_and_docs(Signatures, get_local_doc({F,A},Doc));
                  false ->
                      case lists:keyfind(Group, 1, Docs) of
                          false ->
                              render_headers_and_docs(Signatures, get_local_doc({F,A},none));
                          {_,_,_,Doc,_} ->
                              render_headers_and_docs(Signatures, get_local_doc({F,A},Doc))
                      end
              end
      end, maps:to_list(Grouping)).

render_signature({{_Type,_F,_A},_Anno,_Sigs,_Docs,#{ signature := Specs } = Meta}) ->
    lists:flatmap(
      fun(ASTSpec) ->
              PPSpec = erl_pp:attribute(ASTSpec,[{encoding,utf8}]),
              Spec =
                  case ASTSpec of
                      {_Attribute, _Line, opaque, _} ->
                          %% We do not want show the internals of the opaque type
                          hd(string:split(PPSpec,"::"));
                      _ ->
                          PPSpec
                  end,
              SplittedSpec = string:replace(Spec, "-spec", "\n\n### -spec", all),
              BinSpec =
                  unicode:characters_to_binary(
                    string:trim(SplittedSpec, both, "\n")),
              [{p,[],[BinSpec]}|render_meta(Meta)]
      end, Specs);
render_signature({{_Type,_F,_A},_Anno,Sigs,_Docs,Meta}) ->
    lists:flatmap(
      fun(Sig) ->
              [{h2,[],[<<"  "/utf8,Sig/binary>>]}|render_meta(Meta)]
      end, Sigs).
render_meta(M) ->
    case render_meta_(M) of
        [] -> [];
        Meta ->
            [[{dl,[],Meta}]]
    end.
render_meta_(#{ since := Vsn } = M) ->
    [{dt,[],<<"Since">>},{dd,[],[Vsn]}
    | render_meta_(maps:remove(since, M))];
render_meta_(#{ deprecated := Depr } = M) ->
    [{dt,[],<<"Deprecated">>},{dd,[],[Depr]}
    | render_meta_(maps:remove(deprecated, M))];
render_meta_(_) ->
    [].

render_headers_and_docs(Headers, DocContents) ->
    ["\n",render_docs(
       lists:flatmap(
         fun(Header) ->
                 [{br,[],[]},Header]
         end,Headers)),
     "\n",
     render_docs(DocContents)].

render_docs(DocContents) ->
        Doc = render_docs(DocContents, []),
        Doc.

render_docs(Elems,State) when is_list(Elems) ->
    lists:flatten(lists:map(fun(Elem) ->
                           render_docs(Elem,State)
                   end,Elems));
render_docs(Elem,State) ->
    render_element(Elem,State).

%%% render html element to markdown
render_element({p, _, PContents}, State) ->
    "" ++
    render_elements(PContents, State)
    ++"\n";

render_element({code, _, CodeContents}, State) ->
    {Tag, TagF} = case State of
        [] -> {"``", "``"};
        _ -> case lists:last(State) of
            pre -> { "```\n", "\n```" };
                _ -> {"``", "``"}
            end
    end,
    Code = render_elements(CodeContents, State),
    Tag ++
        string:replace(Code, "\n", "\n\n")
    ++ TagF;

render_element({h2, _, H2Contents}, State) ->
    "## " ++
    render_elements(H2Contents, State)
    ++"";
render_element({h3, _, H2Contents}, State) ->
    "### " ++
    render_elements(H2Contents, State)
    ++"";
render_element({br, _, BrContents}, State) ->
    "\n" ++
    render_elements(BrContents, State)
    ++"";
render_element({ul, _Style, UlContents}, State) ->
    % gen_lsp_server:lsp_log("ul element, style:~p", [_Style]),
    "\n" ++
    render_elements(UlContents, State)
    ++"";
render_element({li, _Style, []}, _State) ->
    "";
render_element({li, _Style, LiContents}, State) ->
    % gen_lsp_server:lsp_log("li element, style:~p, content:~p", [_Style, LiContents]),
    "\n* " ++
    render_elements(LiContents, State)
    ++"";
render_element({a, [{href, _},{rel, _}], AContents}, State) ->
    "(" ++
    render_elements(AContents, State)
    ++")[todo_link]";
render_element({em, _Style, EmContent}, State) ->
    "*" ++
    render_element(EmContent, State)
    ++ "*";
render_element({pre, _Style, PreContents}, State) ->
    "\n"++
    render_elements(PreContents, State ++ [pre])
    ++ "\n\n";

render_element({dl, _Style, DlContents}, State) ->
    render_elements(DlContents, State ++ [dl])
    ;
render_element({dt, _Style, DtContents}, State) ->
    render_elements(DtContents, State)
    ++ "\n";
render_element({dd, _Style, DtContents}, State) ->
    "\n\t"++
    render_elements(DtContents, State)
    ++ "\n";

render_element(Content, _State) when is_binary(Content) ->
    binary_to_list(Content);

render_element(Elem,_State) ->
    gen_lsp_server:lsp_log("unknown element: ~p", [Elem]),
   "".

render_elements(Elems, State) ->
    lists:flatten(
        lists:map(fun(X) -> render_element(X, State) end, Elems)
    ).

get_local_doc(MissingMod, Docs) when is_atom(MissingMod) ->
    get_local_doc(atom_to_binary(MissingMod), Docs);
get_local_doc({F,A}, Docs) ->
    get_local_doc(unicode:characters_to_binary(io_lib:format("~tp/~p",[F,A])), Docs);
get_local_doc(_Missing, #{ <<"en">> := Docs }) ->
    %% English if it exists
    shell_docs:normalize(Docs);
get_local_doc(_Missing, ModuleDoc) when map_size(ModuleDoc) > 0 ->
    %% Otherwise take first alternative found
    shell_docs:normalize(maps:get(hd(maps:keys(ModuleDoc)), ModuleDoc));
get_local_doc(Missing, hidden) ->
    [{p,[],[<<"The documentation for ">>,Missing,
            <<" is hidden. This probably means that it is internal "
              "and not to be used by other applications.">>]}];
get_local_doc(Missing, None) when None =:= none; None =:= #{} ->
    [{p,[],[<<"There is no documentation for ">>,Missing]}].

set_proxy() ->
    % get proxyUrl configuration
    ActualProxy = gen_lsp_config_server:proxy(),
    %systemProxy
    ProxyUrl = lsp_utils:to_string(ActualProxy),
    %error_logger:info_msg("set_proxy : ~p~n", [ProxyUrl]),
    case length(ProxyUrl) of
    L when L > 0 ->
        {ok, {_Scheme, _UserInfo, Host, Port, _Path, _Query}} = parse_uri(ProxyUrl),
        % if proxy is on localhost, NoProxy should not contains localhost 
        NoProxy = case string:to_lower(Host) of
            "localhost" -> [];
            "127.0.0.1" -> [];
            _ -> ["localhost"]
        end,
	    %error_logger:info_msg("proxy info : ~p, ~p~n", [Host, Port]),
        httpc:set_options([{proxy, {{Host, Port}, NoProxy}}]),
        proxy_set;
    _ -> noproxy
    end.

-ifdef(OTP_RELEASE).
parse_uri(Url) ->
    #uri_map{host = Host, port=Port} = uri_string:parse(Url),
    {ok, Host, Port}.
-else.
parse_uri(Url) ->
    {ok, {_Scheme, _UserInfo, Host, Port, _Path, _Query}} = http_uri:parse(Url),
    {ok, Host, Port}.
-endif.

get_page_lines(Module, Modules) ->
    case Modules of
        #{Module := Lines} = _ ->
            {Lines, Modules};
        _ ->
            try set_proxy()
			catch
			_:Err -> error_logger:info_msg("set_proxy failed: ~p~n", [Err])
			end,
            %httpc:set_options([{proxy, {{"www-proxy.mycompany.com", 8000}, ["localhost"]}}]),
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
