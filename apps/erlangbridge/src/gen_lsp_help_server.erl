-module(gen_lsp_help_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_help/2,get_help/5, render_help_body/3]).

-define(SERVER, ?MODULE).

-record(state, {modules}).

-ifdef(OTP_RELEASE).
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
-export([eep48_render_fun_doc/3]).
-record(docs_v1, {docs}).
%% appear in OTP 23.0
atom_to_binary(Atom) ->
    list_to_binary(atom_to_list(Atom)).

-endif.


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

get_help(Module, Function) -> 
    gen_server:call(?SERVER, {get_help, Module, Function}).

get_help(Module, Function, CallBackState, RenderModule, RenderFunction) -> 
    gen_server:call(?SERVER, {get_help, Module, Function, CallBackState, RenderModule, RenderFunction}).

render_help_body(Function, Arity, Doc) ->
    gen_server:call(?SERVER, {render_help_body, Function, Arity, Doc}).

init(_Args) ->
    {ok, #state{modules=#{}}}.

handle_call({get_help, Module, Function}, _From, State) ->
    {reply, safe_get_help(Module, Function, fun eep48_render_fun_doc/4),State};

handle_call({get_help, Module, Function, CallBackState, RenderModule, RenderFunction}, _From, State) ->
    Reply = safe_get_help(Module, Function, fun (M, F, FnDoc, Docs) ->
            apply(RenderModule, RenderFunction, [M, F, FnDoc, Docs, CallBackState])
        end),
    {reply, Reply, State};

handle_call({render_help_body, F, A, Doc}, _From, State) ->
    Reply = render_docs(get_local_doc({F,A},Doc)),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

safe_get_help(Module, Function, Callback) ->
    case get_help_eep48(Module, Function, Callback) of
        {error, _} ->
            undefined;
        Help -> Help
    end.

-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE >= 23).

    get_help_eep48(Module, Function,CallBack) ->
        case code:get_doc(Module) of
            {ok, HelpModule} -> eep48_layout_doc(Module, Function, HelpModule, CallBack);
            _ -> {error, doc_unavailable}
        end.
    -else.
    get_help_eep48(_Module, _Function) ->
        {error, eep48_not_supported}.
    -endif.
-else.
    get_help_eep48(_Module, _Function) ->
        {error, eep48_not_supported}.
-endif.

eep48_layout_doc(_Module, Function, #docs_v1{ docs = Docs } = _D, Callback) ->
    FnDoc = lists:filter(fun({{function, F, _},_Anno,_Sig,_Doc,_Meta}) ->
                             F =:= Function;
                        (_) ->
                             false
                     end, Docs),
    Callback(_Module, Function, FnDoc, Docs).

eep48_render_fun_doc(_Module, _Function, FnDoc, Docs) ->
    list_to_binary(lists:flatten(render_function(FnDoc, Docs))).

render_function([], _D) ->
    {error, function_missing};
render_function(FDocs, Docs) ->
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
    "\n" ++
    render_elements(UlContents, State)
    ++"";
render_element({li, _Style, []}, _State) ->
    "";
render_element({li, _Style, LiContents}, State) ->
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

render_elements(Elems, _State) when is_binary(Elems) ->
    binary_to_list(Elems);

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

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
