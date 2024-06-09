-module(gen_lsp_config_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([standard_modules/0, bifs/0]).
-export([update_config/2, root/0, tmpdir/0, codeLensEnabled/0, includePaths/0, linting/0,
        verbose/0, autosave/0, proxy/0, search_files_exclude/0, search_exclude/0,
        formatting_line_length/0, inlayHintsEnabled/0, verbose_is_include/1]).

-include("lsp_log.hrl").
-define(SERVER, ?MODULE).

-record(state, {config, standard_modules, bifs}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

standard_modules() ->
    gen_server:call(?SERVER, {standard_modules}).

bifs() ->
    gen_server:call(?SERVER, {bifs}).

update_config(Key, Value) ->
    Res = gen_server:call(?SERVER, {update_config, Key, Value}),
    compute_erlang_section(Key),
    Res.

compute_erlang_section(Key) ->
    if 
        Key =:= erlang ->
            Excludes = get_config_entry(erlang, verboseExcludeFilter, ""),
            Splitted = lists:map(fun(X) -> {lsp_utils:to_binary(X), false} end, lists:flatmap( fun(X) -> string:split(X, ",") end, string:split(Excludes, ";"))),
            gen_server:call(?SERVER, {update_config, erlang_computed, #{ verboseExcludeFilter => maps:from_list(Splitted)}});
        true -> ok
    end.


get_config() ->
    gen_server:call(?SERVER, get_config).

get_config_entry(Section, Entry, Default) ->
    SectionMap = maps:get(Section, get_config(), #{}),
    maps:get(Entry, SectionMap, Default).

root() ->
    maps:get(root, get_config(), "").

codeLensEnabled() ->
    get_config_entry(erlang, codeLensEnabled, false).

inlayHintsEnabled() ->
    get_config_entry(erlang, inlayHintsEnabled, false).

includePaths() ->
    get_config_entry(erlang, includePaths, []).

linting() ->
    get_config_entry(erlang, linting, true).

verbose() ->
    get_config_entry(erlang, verbose, false).

verbose_is_include(Method) ->
    M = get_config_entry(erlang_computed, verboseExcludeFilter, #{}),
    lsp_utils:try_get(Method, M, true).

autosave() ->
    get_config_entry(computed, autosave, true).

proxy() ->
    get_config_entry(http, proxy, "").

tmpdir() ->
    get_config_entry(computed, tmpdir, "").

%%--------------------------------------------------------------------
%% @doc Exclude filters for search in workspace.
%%
%% It is a combination of Visual Studio Code settings `files.exclude' and
%% `search.exclude' as Visual Studio Code GUI does merge these for searching
%% but extensions get the pure settings and we have to merge them explicitly.
%% @end
%%--------------------------------------------------------------------
search_exclude() ->
    %% From https://code.visualstudio.com/docs/getstarted/settings
    %% files.exclude:
    %%   Configure glob patterns for excluding files and folders. For example,
    %%   the File Explorer decides which files and folders to show or hide based
    %%   on this setting. Refer to the `search.exclude` setting to define
    %%   search-specific excludes.
    %% search.exclude:
    %%   Configure glob patterns for excluding files and folders in fulltext
    %%   searches and quick open. Inherits all glob patterns from the
    %%   `files.exclude` setting.
    maps:merge(get_config_entry(files, exclude, #{}),
               get_config_entry(search, exclude, #{})).

%%--------------------------------------------------------------------
%% @doc Exclude filters for searching project files.
%%
%% It is a combination of Visual Studio Code settings `files.exclude',
%% `files.watcherExclude' and `search.exclude'.
%% @end
%%--------------------------------------------------------------------
search_files_exclude() ->
    %% From https://code.visualstudio.com/docs/getstarted/settings
    %% files.watcherExclude:
    %%   Configure paths or glob patterns to exclude from file watching.
    maps:merge(get_config_entry(files, watcherExclude, #{}), search_exclude()).

formatting_line_length() ->
    get_config_entry(erlang, formattingLineLength, 100).

init(_Args) ->
    StandardModules = lists:foldl(fun (Dir, Acc) ->
        lists:foldl(fun (File, AccF) ->
            [filename:rootname(File) | AccF]
        end, Acc, filelib:wildcard("*.beam", Dir))
    end, [], code:get_path()),
    BIFs = sets:to_list(lists:foldl(fun ({Name, _Arity}, Acc) ->
        sets:add_element(atom_to_list(Name), Acc)
    end, sets:new(), erlang:module_info(exports))),
    {ok, #state{config = #{}, standard_modules = StandardModules, bifs = BIFs}}.

handle_call({standard_modules}, _From, State) ->
    {reply, State#state.standard_modules, State};
handle_call({bifs}, _From, State) ->
    {reply, State#state.bifs, State};
handle_call({update_config, Key, Value}, _From, State) ->
    {reply, #{}, State#state{config = (State#state.config)#{Key => Value}}};
handle_call(get_config, _From, State) ->
    {reply, State#state.config, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
