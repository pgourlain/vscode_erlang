-module(gen_lsp_config_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([update_config/2, root/0, tmpdir/0, codeLensEnabled/0, includePaths/0, linting/0, verbose/0, autosave/0]).

-define(SERVER, ?MODULE).

-record(state, {config}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

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

tmpdir() ->
    get_config_entry(computed, tmpdir, "").

init(_Args) ->
    {ok, #state{config = #{}}}.

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
