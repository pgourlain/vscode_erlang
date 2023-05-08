-module(gen_lsp_doc_server).

-behavior(gen_server).
-export([start_link/0]).

-export([document_opened/2, document_changed/2, document_closed/1, opened_documents/0, get_document_contents/1, parse_document/1]).
-export([project_file_added/1, project_file_changed/1, project_file_deleted/1]).
-export([get_syntax_tree/1, get_dodged_syntax_tree/1]).
-export([root_available/0, project_modules/0, get_module_file/1, get_build_dir/0]).
-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {project_modules, files_to_parse}).

document_opened(File, Contents) ->
    ets:insert(document_contents, {File, Contents}).

document_changed(File, Contents) ->
    ets:insert(document_contents, {File, Contents}).

document_closed(File) ->
    ets:delete(document_contents, File).

opened_documents() ->
    [File || {File, _Contents} <- ets:tab2list(document_contents)].

get_document_contents(File) ->
    case ets:lookup(document_contents, File) of
        [{File, Contents}] -> Contents;
        _ -> undefined
    end.

parse_document(File) ->
    case filename:extension(File) of
        ".erl" ->
            case get_document_contents(File) of
                undefined ->
                    io:format("Cannot find contents of document ~p~n", [File]);
                Contents ->
                    ContentsFile = lsp_utils:make_temporary_file(Contents),
                    parse_and_store_trees(File, ContentsFile),
                    file:delete(ContentsFile)
            end;
        _ ->
            ok
    end.

project_file_added(File) ->
    gen_server:cast(?SERVER, {project_file_added, File}).

project_file_changed(File) ->
    gen_server:cast(?SERVER, {project_file_changed, File}).

project_file_deleted(File) ->
    gen_server:cast(?SERVER, {project_file_deleted, File}).

get_syntax_tree(File) ->
    case get_tree(syntax_tree, File) of
        undefined ->
            parse_and_store_trees(File, File),
            get_tree(syntax_tree, File);
        SyntaxTree ->
            SyntaxTree
    end.

get_dodged_syntax_tree(File) ->
    case get_tree(dodged_syntax_tree, File) of
        undefined ->
            parse_and_store_trees(File, File),
            get_tree(dodged_syntax_tree, File);
        SyntaxTree ->
            SyntaxTree
    end.

root_available() ->
    gen_server:cast(?SERVER, root_available).

project_modules() ->
    gen_server:call(?SERVER, project_modules).

get_module_file(Module) ->
    gen_server:call(?SERVER, {get_module_file, Module}).

get_build_dir() ->
    ConfigFilename = filename:join([gen_lsp_config_server:root(), "rebar.config"]),
    case filelib:is_file(ConfigFilename) of
        true ->
            Default = "_build",
            case file:consult(ConfigFilename) of
                {ok, Config} ->
                    proplists:get_value(base_dir, Config, Default);
                {error, _} ->
                    Default
            end;
        false ->
            undefined
    end.

start_link() ->
    safe_new_table(document_contents),
    safe_new_table(syntax_tree),
    safe_new_table(dodged_syntax_tree),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

init(_Args) ->
    {ok, #state{project_modules = #{}, files_to_parse = []}}.

handle_call(project_modules, _From, State) ->
    {reply, maps:keys(State#state.project_modules), State};

handle_call({get_module_file, Module},_From, State) ->
    %% Get search.exclude setting of Visual Studio Code
    SearchExcludeConf = gen_lsp_config_server:search_exclude(),
    SearchExclude = lsp_utils:search_exclude_globs_to_regexps(SearchExcludeConf),
    %% Select a non-excluded file
    Files = maps:get(atom_to_list(Module), State#state.project_modules, []),
    File =
        case [F || F<-Files, not lsp_utils:is_path_excluded(F, SearchExclude)] of
            []          -> 
                %try to find in erlang source files
                case filelib:wildcard(code:lib_dir()++"/**/" ++ atom_to_list(Module) ++ ".erl") of
                    [] -> undefined;
                    [OneFile]   -> OneFile;
                    [AFile | _] -> AFile
                end;
            [OneFile]   -> OneFile;
            [AFile | _] -> AFile
        end,
    {reply, File, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(root_available, State) ->
    BuildDir = get_build_dir(),
    Fun = fun(File, {ProjectModules, FilesToParse}) ->
        {do_add_project_file(File, ProjectModules, BuildDir), [File | FilesToParse]}
    end,
    {ProjectModules, FilesToParse} = filelib:fold_files(gen_lsp_config_server:root(), ".erl$", true, Fun, {#{}, []}),
    {noreply, parse_next_file_in_background(State#state{project_modules = ProjectModules, files_to_parse = FilesToParse})};

handle_cast({project_file_added, File}, State) ->
    UpdatedProjectModules = do_add_project_file(File, State#state.project_modules, get_build_dir()),
    FilesToParse = [File | State#state.files_to_parse],
    UpdatedState = State#state{project_modules = UpdatedProjectModules, files_to_parse = FilesToParse},
    {noreply, parse_next_file_in_background(UpdatedState)};

handle_cast({project_file_changed, File}, State) ->
    case ets:lookup(document_contents, File) of
        [_FileContents] ->
            {noreply, State};
        _ ->
            FilesToParse = [File | State#state.files_to_parse],
            {noreply, parse_next_file_in_background(State#state{files_to_parse = FilesToParse})}
    end;

handle_cast({project_file_deleted, File}, State) ->
    ets:delete(document_contents, File),
    ets:delete(syntax_tree, File),
    ets:delete(dodged_syntax_tree, File),
    Module = filename:rootname(filename:basename(File)),
    UpdatedFiles = lists:delete(File, maps:get(Module, State#state.project_modules, [])),
    UpdatedProjectModules = case UpdatedFiles of
        [] -> maps:remove(Module, State#state.project_modules);
        _ -> (State#state.project_modules)#{Module => UpdatedFiles}
    end,
    {noreply, State#state{project_modules = UpdatedProjectModules}}.

handle_info({worker_result, File, _Result}, State) ->
    gen_lsp_server:lsp_log("Parsed in background: ~s~n", [File]),
    {noreply, parse_next_file_in_background(State)};
handle_info({worker_error, File, _Exception}, State) ->
    gen_lsp_server:lsp_log("Parse in background failed: ~s~n", [File]),
    {noreply, parse_next_file_in_background(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

do_add_project_file(File, ProjectModules, BuildDir) ->
    Module = filename:rootname(filename:basename(File)),
    UpdatedFiles = concat_project_files(File, maps:get(Module, ProjectModules, []), BuildDir),
    ProjectModules#{Module => UpdatedFiles}.

concat_project_files(File, OldFiles, undefined) ->
    [File | OldFiles];
concat_project_files(File, OldFiles, BuildDir) ->
    case lists:member(BuildDir, filename:split(File)) of
        true  -> OldFiles ++ [File];
        false -> [File | OldFiles]
    end.

safe_new_table(Name) ->
    case ets:whereis(Name) of
        undefined -> ets:new(Name, [named_table, public]);
        _ -> Name
    end.

parse_and_store_trees(File, ContentsFile) ->
    {SyntaxTree, DodgedSyntaxTree} = lsp_parse:parse_source_file(File, ContentsFile),
    case SyntaxTree of
        undefined -> ok;
        _ -> ets:insert(syntax_tree, {File, SyntaxTree})
    end,
    case DodgedSyntaxTree of
        undefined -> ok;
        _ -> ets:insert(dodged_syntax_tree, {File, DodgedSyntaxTree})
    end.

get_tree(TreeType, File) ->
    case ets:lookup(TreeType, File) of
        [{File, SyntaxTree}] ->
            SyntaxTree;
        _ ->
            undefined
    end.

parse_next_file_in_background(#state{files_to_parse = []} = State) ->
    State;
parse_next_file_in_background(#state{files_to_parse = [File | Rest]} = State) ->
    worker:start(fun () -> parse_and_store_trees(File, File) end, File),
    State#state{files_to_parse = Rest}.
