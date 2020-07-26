-module(gen_lsp_config_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([standard_modules/0, bifs/0]).
-export([update_config/2, root/0, tmpdir/0, codeLensEnabled/0, includePaths/0, linting/0, 
        verbose/0, autosave/0, proxy/0, search_exclude/0, eep48_help/0]).

-define(SERVER, ?MODULE).

-record(state, {config, standard_modules, bifs}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

standard_modules() ->
    gen_server:call(?SERVER, {standard_modules}).

bifs() ->
    gen_server:call(?SERVER, {bifs}).

update_config(Key, Value) ->
    gen_server:call(?SERVER, {update_config, Key, Value}).

get_config() ->
    gen_server:call(?SERVER, get_config).

get_config_entry(Section, Entry, Default) ->
    SectionMap = maps:get(Section, get_config(), #{}),
    maps:get(Entry, SectionMap, Default).

root() ->
    maps:get(root, get_config(), "").

codeLensEnabled() ->
    get_config_entry(erlang, codeLensEnabled, false).

includePaths() ->
    get_config_entry(erlang, includePaths, []).

linting() ->
    get_config_entry(erlang, linting, true).

verbose() ->
    get_config_entry(erlang, verbose, false).

autosave() ->
    get_config_entry(computed, autosave, true).

proxy() ->
    get_config_entry(http, proxy, "").

tmpdir() ->
    get_config_entry(computed, tmpdir, "").

search_exclude() ->
    get_config_entry(search, exclude, #{}).

eep48_help() ->
    get_config_entry(erlang, eep48Help, false).

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
