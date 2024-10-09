-module(gen_lsp_doc_server).
-behavior(gen_server).

%% API
-export([start_link/0]).

-export([document_opened/2, document_changed/2, document_closed/1, opened_documents/0, get_document_contents/1, parse_document/1]).
-export([project_file_added/1, project_file_changed/1, project_file_deleted/1]).
-export([get_syntax_tree/1, get_dodged_syntax_tree/1, get_references/1, get_inlayhints/1]).
-export([root_available/0, config_change/0, project_modules/0, get_module_file/1, get_module_files/1, get_build_dir/0, find_source_file/1]).

%% Cache management
-export([delete_unused_caches/2,
         persist_cache_mgmt_opts/0]).

%% gen_server callbacks
-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("./lsp_log.hrl").

-define(SERVER, ?MODULE).
-define(XETS, (persistent_term:get(large_cache_module, ets))).
-define(IIF(Cond, Then, Else), if Cond -> Then; true -> Else end).

-record(state,
        {root_available = false :: boolean(),
         project_modules = #{} :: #{atom() => [file:filename()]},
         files_to_parse = [] :: [file:filename()]
        }).

document_opened(File, Contents) ->
    ?XETS:insert(document_contents, {File, Contents}).

document_changed(File, Contents) ->
    ?XETS:insert(document_contents, {File, Contents}).

document_closed(File) ->
    ?XETS:delete(document_contents, File).

opened_documents() ->
    do_opened_documents(?XETS).

do_opened_documents(ets) ->
    [File || {File, _Contents} <- ets:tab2list(document_contents)];
do_opened_documents(dets) ->
    dets:traverse(document_contents,
                  fun({File, _Contents}) -> {continue, File};
                      (_) -> continue
                  end).

get_document_contents(File) ->
    case ?XETS:lookup(document_contents, File) of
        [{File, Contents}] -> Contents;
        _ -> undefined
    end.

parse_document(File) ->
    case filename:extension(File) of
        ".erl" ->
            case get_document_contents(File) of
                undefined ->
                    error_logger:error_msg("Cannot find contents of document ~p~n", [File]);
                Contents ->
                    ContentsFile = lsp_utils:make_temporary_file(Contents),
                    parse_and_store(File, ContentsFile),
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
            parse_and_store(File, File),
            get_tree(syntax_tree, File);
        SyntaxTree ->
            SyntaxTree
    end.

get_dodged_syntax_tree(File) ->
    case get_tree(dodged_syntax_tree, File) of
        undefined ->
            parse_and_store(File, File),
            get_tree(dodged_syntax_tree, File);
        SyntaxTree ->
            SyntaxTree
    end.

get_references(Reference) ->
    ets:match(references, {'$1', Reference, '$2', '$3', '$4'}).

get_inlayhints(File) ->
    case ?XETS:lookup(document_inlayhints, File) of
        [{File, Inlays}] -> Inlays;
        _ -> []
    end.

root_available() ->
    gen_server:cast(?SERVER, root_available).

config_change() ->
    gen_server:cast(?SERVER, config_change).

project_modules() ->
    gen_server:call(?SERVER, project_modules).

get_module_file(Module) ->
    gen_server:call(?SERVER, {get_module_file, Module}).

get_module_files(Module) ->
    gen_server:call(?SERVER, {get_module_files, Module}).

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

find_source_file(File) ->
    BuildDir = filename:join(gen_lsp_config_server:root(), gen_lsp_doc_server:get_build_dir()),
    Result = lists:filter(fun (Path) ->
        string:prefix(Path, BuildDir) =:= nomatch
    end, filelib:wildcard(as_string(filename:join([gen_lsp_config_server:root(), "**", File])))),
    case Result of
        [AFile | _] -> AFile;
        [] -> undefined
    end.

as_string(Text) when is_binary(Text) ->
    binary_to_list(Text);
as_string(Text) -> 
    Text.

start_link() ->
    ExtraCreateOpts = persistent_term:get(large_cache_create_opts, []),
    safe_new_table(document_contents, ?XETS, set, ExtraCreateOpts),
    safe_new_table(syntax_tree, ?XETS, set, ExtraCreateOpts),
    safe_new_table(dodged_syntax_tree, ?XETS, set, ExtraCreateOpts),
    safe_new_table(references, ets, bag, []),
    safe_new_table(document_inlayhints, ?XETS, set, ExtraCreateOpts),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

init(_Args) ->
    process_flag(trap_exit, true), % to terminate/2 be called at exit
    {ok, #state{root_available = false, project_modules = #{}, files_to_parse = []}}.

handle_call(project_modules, _From, State) ->
    {reply, maps:keys(State#state.project_modules), State};

handle_call({get_module_file, Module},_From, State) ->
    case find_module_files(Module, State) of
        [AFile | _] -> {reply, AFile, State};
        []          -> {reply, undefined, State}
    end;

handle_call({get_module_files, Module}, _From, State) ->
    {reply, find_module_files(Module, State), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(root_available, State) ->
    {noreply, State#state{root_available = true}};

handle_cast(config_change, State = #state{root_available = true}) ->
    {noreply, scan_project_files(State)};
handle_cast(config_change, State) ->
    {noreply, State};

handle_cast({project_file_added, File}, State) ->
    UpdatedProjectModules = do_add_project_file(File, State#state.project_modules, get_build_dir()),
    FilesToParse = [File | State#state.files_to_parse],
    UpdatedState = State#state{project_modules = UpdatedProjectModules, files_to_parse = FilesToParse},
    {noreply, parse_next_file_in_background(UpdatedState)};

handle_cast({project_file_changed, File}, State) ->
    case ?XETS:lookup(document_contents, File) of
        [_FileContents] ->
            {noreply, State};
        _ ->
            FilesToParse = [File | State#state.files_to_parse],
            {noreply, parse_next_file_in_background(State#state{files_to_parse = FilesToParse})}
    end;

handle_cast({project_file_deleted, File}, State) ->
    {noreply, delete_project_files([File], State)}.

handle_info({worker_result, File, _Result}, State) ->
    ?LOG("Parsed in background: ~s~n", [unicode:characters_to_binary(File)]),
    {noreply, parse_next_file_in_background(State)};
handle_info({worker_error, File, _Exception}, State) ->
    ?LOG("Parse in background failed: ~s~n", [unicode:characters_to_binary(File)]),
    {noreply, parse_next_file_in_background(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    delete_cache_file(document_contents),
    delete_cache_file(syntax_tree),
    delete_cache_file(dodged_syntax_tree),
    delete_cache_file(document_inlayhints),
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
    case is_file_in_rebar_build_dir(File, BuildDir) of
        true  -> OldFiles ++ [File];
        false -> [File | OldFiles]
    end.

%% @doc Check if a file is located under rebar3 build directory.
-spec is_file_in_rebar_build_dir(File :: file:filename(),
                                 BuildDir :: file:filename())
                                -> boolean().
is_file_in_rebar_build_dir(File, BuildDir) ->
    lists:member(BuildDir, filename:split(File)).

%%--------------------------------------------------------------------
%% @doc Filter source files of a module that shall be parsed.
%%
%% Multiple source files could have been found for a module. For example after
%% `rebar3 compile' the original source files are sym-linked from the build
%% directory (`_build'). In this case ignore the linked file but parse the
%% originals only.
%% @end
%%--------------------------------------------------------------------
-spec module_files_to_parse(Files :: [file:filename()],
                            BuildDir :: file:filename())
                        -> FilesToParse :: [file:filename()].
module_files_to_parse([], _BuildDir) ->
    [];
module_files_to_parse(Files, BuildDir) ->
    IsRebarBuildFileFun =
        fun(File) -> is_file_in_rebar_build_dir(File, BuildDir) end,
    case lists:partition(IsRebarBuildFileFun, Files) of
        {[], _NonBuildFiles} ->
            %% All files are from this repo, parse them all
            Files;
        {_BuildFiles, []} ->
            %% All files are from dependencies.
            %% In case of multiple files are found for a module and all of them
            %% are in the build directory (_build) then most probably those are
            %% the same but located under different rebar3 release targets.
            %% Which one to choose? :S
            %% Choose the first, if all are the same. (But are they?)
            [hd(lists:sort(Files))]; % sort just to choose the 1st consistently
        {_, NonBuildFiles} ->
            %% Files are under both '_build' and repo. The files under
            %% '_build' are sym-linked by rebar3, ignore these.
            %% Parse the original ones only.
            NonBuildFiles
    end.

%%--------------------------------------------------------------------
%% @doc Return automatic exclude filters in case of no corresponding
%% configuration.
%%
%% If there is no exclude filter specified for some paths (neither by user, host
%% or workspace) then we shall use some automatic exclude filters to exclude
%% some usually unwanted paths from searching and parsing source files.
%%
%% Example:
%% Bazel build system sym-links dependencies of test (maybe all) rules with
%% unique paths through `bazel-*' subdirectories of the workspace.
%% It can results large number of duplicated source files, like Erlang/OTP,
%% that are parsed and stored in ETS tables and consumes huge amount of memory.
%% We don't need to parse these files, at least not the duplicates. One
%% occurrence per source file is enough but as every project is different, we
%% cannot come up a universal exclude/include filter list, so let's exclude
%% Bazel cache directories.
%% @end
%%--------------------------------------------------------------------
-spec get_auto_exclude_filters() -> [{Glob, DoExclude, InverseRegExpConditions}]
      when Glob :: string(),
           DoExclude :: boolean(),
           InverseRegExpConditions :: [RegularExpression],
           RegularExpression :: string().
get_auto_exclude_filters() ->
    [%% Bazel cache
     {"**/bazel-*/**", true, ["^(.*[/\\\\])?bazel-[^/\\\\]+([/\\\\].*)?$"]}
    ].

%%--------------------------------------------------------------------
%% @doc Exclude filters for searching and parsing project files.
%%
%% It is basically the same as
%% {@link gen_lsp_config_server:search_files_exclude/0} with some extra exclude
%% filters for special build systems or other tools if there is no corresponding
%% configuration for those.
%% @end
%% @see get_auto_exclude_filters/0
%% @see gen_lsp_config_server:search_files_exclude/0
%%--------------------------------------------------------------------
-spec get_scan_project_files_exclude_conf() ->
        #{Glob :: atom() => DoExclude :: boolean()}.
get_scan_project_files_exclude_conf() ->
    SearchExcludeConf = gen_lsp_config_server:search_files_exclude(),
    SearchExcludes = [{atom_to_list(Glob), DoExcl}
                      || {Glob, DoExcl}<-maps:to_list(SearchExcludeConf)],
    AutoExcludes = get_auto_exclude_filters(),
    CompiledAutoExcludes =
        [{Glob, DoExclude,
          [begin {ok, MP} = re:compile(RE), MP end || RE<-InvRegExpConditions]}
         || {Glob, DoExclude, InvRegExpConditions}<-AutoExcludes],
    NewSearchExcludes =
        lists:foldl(
            fun({Glob, DoExclude, InverseREs}, AccSearchExcludes) ->
                %% Check all exclude filter if matches to any regexp
                HasMatchingGlob =
                    lists:any(
                        fun({AccGlob, _AccDoExclude}) ->
                            lists:any(
                                fun(RE) -> nomatch /= re:run(AccGlob, RE) end,
                                InverseREs)
                        end,
                        AccSearchExcludes),
                case HasMatchingGlob of
                    true -> %% There are some matching globs, trust VSC settings
                        AccSearchExcludes;
                    false -> %% No glob is specified, add the automatic one
                        [{Glob, DoExclude} | AccSearchExcludes]
                end
            end,
            SearchExcludes,
            CompiledAutoExcludes),
    maps:from_list(
        [{list_to_atom(Glob), DoExcl} || {Glob, DoExcl}<-NewSearchExcludes]).

%% @doc Scan workspace for project source files including dependencies too.
-spec scan_project_files(#state{}) -> #state{}.
scan_project_files(State = #state{project_modules = OldProjectModules}) ->
    BuildDir = get_build_dir(), % relative to workspace root or 'undefined'
    SearchExcludeConf = get_scan_project_files_exclude_conf(),
    SearchExclude = lsp_utils:search_exclude_globs_to_regexps(SearchExcludeConf),
    %% Find all source (*.erl) files not excluded by any filter
    CollectProjSrcFilesFun =
        fun(File, AccProjectModules = #{}) ->
            % File can contain unicode characters (e.g. smiley in the path, like in otp repo)            
            case lsp_utils:is_path_excluded(unicode:characters_to_binary(File), SearchExclude) of
                true  -> AccProjectModules;
                false -> do_add_project_file(File, AccProjectModules, BuildDir)
            end
        end,
    AllProjectModules = filelib:fold_files(gen_lsp_config_server:root(), "\\.erl$",
                                           true, CollectProjSrcFilesFun, #{}),
    NbFiles = maps:size(AllProjectModules),
    LogAllProjectModules = ?IIF(NbFiles > 200, #{}, AllProjectModules),
    ?LOG(
        "~p: Project modules (~p) with all source files:~n  ~p~n",
        [?MODULE, NbFiles, LogAllProjectModules]),
    %% Filter out source files sym-linked to the original ones by rebar3
    %% from '_build' directory
    ProjectModules =
        maps:map(
            fun(_Module, Files) -> module_files_to_parse(Files, BuildDir) end,
            AllProjectModules),
    
    NbPrjFiles = maps:size(ProjectModules),
    LogProjectModules = ?IIF(NbPrjFiles > 200, #{}, ProjectModules),
    ?LOG(
        "~p: Project modules (~p) without rebar3 duplicated source files:~n  ~p~n",
        [?MODULE, maps:size(ProjectModules), LogProjectModules]),
    %% Get the complete list of source files (already scanned and newly found)
    OldProjFiles = lists:usort(lists:append(maps:values(OldProjectModules))),
    ProjFiles = lists:usort(lists:append(maps:values(ProjectModules))),
    %% Drop unwanted files, e.g. newly excluded by a filter change
    FilesToDrop = OldProjFiles -- ProjFiles,
    ?LOG(
        "~p: Files to drop: (~p)~n  ~p~n",
        [?MODULE, length(FilesToDrop), lists:sort(FilesToDrop)]),
    State2 = delete_project_files(FilesToDrop, State),
    %% Get files not parsed yet
    FilesToParse = ProjFiles -- OldProjFiles,
    NbFileToParse = length(FilesToParse),
    LogFilesToParse = ?IIF(NbFileToParse > 200, lists:sublist(FilesToParse,200), FilesToParse),
    ?LOG(
        "~p: Files to parse: (~p)~n  ~p~n",
        [?MODULE, length(FilesToParse), lists:sort(LogFilesToParse)]),

    %% Load the 1st source file (and continue later one-by-one ...)
    NewState = State2#state{project_modules = ProjectModules,
                            files_to_parse = FilesToParse},
    parse_next_file_in_background(NewState).

%% @doc Remove files from the navigation database.
-spec delete_project_files(file:filename(), #state{}) -> #state{}.
delete_project_files([], State) ->
    State;
delete_project_files([File | Files], State) ->
    ?XETS:delete(document_contents, File),
    ?XETS:delete(syntax_tree, File),
    ?XETS:delete(dodged_syntax_tree, File),
    ets:delete(references, File),
    ?XETS:delete(document_inlayhints, File),
    Module = filename:rootname(filename:basename(File)),
    UpdatedFiles = lists:delete(File, maps:get(Module, State#state.project_modules, [])),
    UpdatedProjectModules = case UpdatedFiles of
        [] -> maps:remove(Module, State#state.project_modules);
        _ -> (State#state.project_modules)#{Module => UpdatedFiles}
    end,
    NewState = State#state{project_modules = UpdatedProjectModules},
    delete_project_files(Files, NewState).

%%--------------------------------------------------------------------
%% @private
%% @doc Create a new ETS or DETS table owned by the supervisor.
%%
%% This function is called by {@link start_link/0} that is called from the
%% supervisor process, therefore created ETS and DETS tables are owned by the
%% supervisor instead of the worker `gen_server' process. And so, if the worker
%% process is crashed and restarted then data is still available in the original
%% table.
%% @end
%%--------------------------------------------------------------------
safe_new_table(Name, ets, Type, ExtraCreateOpts) ->
    case ets:whereis(Name) of
        undefined ->
            ets:new(Name, [Type, named_table, public | ExtraCreateOpts]),
            Name;
        _ ->
            %% Supervisor still holds the ETS table
            Name
    end;
safe_new_table(Name, dets, Type, ExtraCreateOpts) ->
    case dets:info(Name, filename) of
        undefined ->
            CacheDir = persistent_term:get(large_cache_dets_dir),
            FileName = filename:join(CacheDir, atom_to_list(Name)++".dets"),
            OpenOpts = [{type, Type}, {file, FileName} | ExtraCreateOpts],
            filelib:ensure_dir(FileName),
            dets:open_file(Name, OpenOpts),
            % dets:delete_all_objects(Name),
            Name;
        _ ->
            %% Supervisor still holds the DETS table open
            Name
    end.

delete_cache_file(Name) ->
    case dets:info(Name, filename) of
        FileName ->
            dets:close(Name),
            file:delete(FileName);
        _ ->
            ok
    end.

parse_and_store(File, ContentsFile) ->
    {SyntaxTree, DodgedSyntaxTree} = lsp_parse:parse_source_file(File, ContentsFile),
    case SyntaxTree of
        undefined ->
            ok;
        _ ->
            ?XETS:insert(syntax_tree, {File, SyntaxTree}),
            ets:delete(references, File),
            ?XETS:delete(document_inlayhints, File),
            lsp_navigation:fold_references(fun (Reference, Line, Column, End, _) ->
                ets:insert(references, {File, Reference, Line, Column, End})
            end, undefined, File, SyntaxTree),
            ?XETS:insert(document_inlayhints, {File, lsp_navigation:full_inlayhints_info(File,SyntaxTree, DodgedSyntaxTree)})
    end,
    case DodgedSyntaxTree of
        undefined -> ok;
        _ -> ?XETS:insert(dodged_syntax_tree, {File, DodgedSyntaxTree})
    end.

get_tree(TreeType, File) ->
    case ?XETS:lookup(TreeType, File) of
        [{File, SyntaxTree}] ->
            SyntaxTree;
        _ ->
            undefined
    end.

parse_next_file_in_background(#state{files_to_parse = []} = State) ->
    State;
parse_next_file_in_background(#state{files_to_parse = [File | Rest]} = State) ->
    worker:start(fun () -> parse_and_store(File, File) end, File),
    State#state{files_to_parse = Rest}.

%% @doc Find source files of a module
-spec find_module_files(module(), #state{}) -> [file:filename()].
find_module_files(Module, State) ->
    case maps:get(atom_to_list(Module), State#state.project_modules, []) of
        [_|_] = Files ->
            Files;
        [] ->
            case find_module_files_under_dir(Module, code:lib_dir()) of
                [] ->
                    BuildDir = filename:join(gen_lsp_config_server:root(),
                                             gen_lsp_doc_server:get_build_dir()),
                    find_module_files_under_dir(Module, BuildDir);
                [_|_] = LibFiles ->
                    LibFiles
            end
    end.

%% @doc Find source files of a module under a certain path
-spec find_module_files_under_dir(module(), file:filename()) -> [file:filename()].
find_module_files_under_dir(Module, Dir) ->
    filelib:wildcard(Dir ++ "/**/" ++ atom_to_list(Module) ++ ".erl").

%%%-------------------------------------------------------------------
%%% Cache management
%%%-------------------------------------------------------------------

persist_cache_mgmt_opts() ->
    do_persist_cache_mgmt_opts(init:get_argument(vscode_cache_mgmt)).

do_persist_cache_mgmt_opts({ok, [["memory"]]}) ->
    persistent_term:put(large_cache_module, ets),
    persistent_term:put(large_cache_create_opts, []);
do_persist_cache_mgmt_opts({ok, [["memory", "compressed"]]}) ->
    persistent_term:put(large_cache_module, ets),
    persistent_term:put(large_cache_create_opts, [compressed]);
do_persist_cache_mgmt_opts({ok, [["file", UserName, TmpDir]]}) ->
    persistent_term:put(large_cache_module, dets),
    persistent_term:put(large_cache_create_opts, []),
    persistent_term:put(large_cache_dets_dir, cache_dir(TmpDir, UserName, os:getpid()));
do_persist_cache_mgmt_opts(_) ->
    do_persist_cache_mgmt_opts({ok, [["memory"]]}).

cache_dir(TmpDir, UserName, OsPid) ->
    filename:join(cache_basedir(TmpDir, UserName), OsPid).

cache_basedir(TmpDir, UserName) ->
    filename:join([TmpDir, "vscode_erlang_"++UserName, "cache"]).

%%--------------------------------------------------------------------
%% @doc Delete all cache directories that are not in use any more.
%% It practically means, delete caches of those extension instances that were
%% terminated without executing proper cleanup (e.g. killed by OS).
%% @end
%%--------------------------------------------------------------------
-spec delete_unused_caches(TmpDir :: string(), UserName :: string()) -> ok.
delete_unused_caches(TmpDir, UserName) when is_binary(TmpDir) ->
    delete_unused_caches(binary_to_list(TmpDir), UserName);
delete_unused_caches(TmpDir, UserName) when is_binary(UserName) ->
    delete_unused_caches(TmpDir, binary_to_list(UserName));
delete_unused_caches(TmpDir = [_|_], UserName = [_|_]) ->
    try
        CacheOsPids = get_cache_os_pids(TmpDir, UserName),
        %% NOTE: a non-Erlang process may exists with the same PID as an old
        %% extension instance.
        DeadCacheOsPids = filter_non_existent_os_pids(CacheOsPids),
        do_delete_unused_caches(TmpDir, UserName, DeadCacheOsPids)
    catch Class:Reason:StackTrace ->
        error_logger:error_report([{Class, Reason}, {stacktrace, StackTrace}])
    end;
delete_unused_caches(_TmpDir, _UserName) ->
    ok.

do_delete_unused_caches(TmpDir, UserName, OsPidsToRemove) ->
    lists:foreach(
        fun(OsPid) ->
            file:del_dir_r(cache_dir(TmpDir, UserName, OsPid))
        end,
        OsPidsToRemove).

%% Return OS PIDs of Erlang VMs, executing extension instances by the current
%% user, that did create cache directories, regardless if the processes are
%% alive or not.
-spec get_cache_os_pids(TmpDir :: string(), UserName :: string())
        -> OsPids :: [string()].
get_cache_os_pids(TmpDir, UserName) ->
    CacheBaseDir = cache_basedir(TmpDir, UserName),
    case file:list_dir(CacheBaseDir) of
        {ok, Filenames} ->
            lists:filter(
                fun(FN) -> filelib:is_dir(filename:join(CacheBaseDir, FN)) end,
                Filenames);
        _ ->
            []
    end.

%% Return OS PIDs that do not belong to any live OS process.
-spec filter_non_existent_os_pids(OsPids) -> OsPids
      when OsPids :: [string()].
filter_non_existent_os_pids([]) ->
    [];
filter_non_existent_os_pids(OsPids) ->
    case os:type() of
        {win32,_} -> filter_non_existent_win32_pids(OsPids);
        {unix, _} -> filter_non_existent_unix_pids(OsPids)
    end.

filter_non_existent_unix_pids(OsPids) ->
    lists:filter(
        fun(OsPid) -> not filelib:is_dir("/proc/" ++ OsPid) end,
        OsPids).

filter_non_existent_win32_pids(OsPids) ->
    %% In Windows there is no similar thing like /proc/<PID> filesystem entries
    %% in Unix systems, but it's still possible to list all running processes.
    WindowsPids = get_win32_pids(),
    lists:filter(
        fun(OsPid) -> not maps:is_key(OsPid, WindowsPids) end,
        OsPids).

%% Return the PIDs of alive Windows process.
-spec get_win32_pids() -> #{OsPid :: string() => 1}.
get_win32_pids() ->
    lists:foldl(
        fun(Line, Acc) ->
            case string:split(Line, "\",\"", all) of
                [_, OsPid | _] -> Acc#{OsPid => 1};
                _              -> Acc
            end
        end,
        #{},
        string:lexemes(os:cmd("tasklist /FO CSV /NH"), ["\r\n", $\r, $\n])).
