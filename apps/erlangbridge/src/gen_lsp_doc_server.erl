-module(gen_lsp_doc_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_or_update_document/2, remove_document/1, get_document/1, set_rebar_config/1, get_rebar_config/0]).

-define(SERVER, ?MODULE).

-record(state, {dict, rebar_config}).

start_link() ->
    error_logger:info_msg("~p:start_link()", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

add_or_update_document(Uri, Document) -> 
    gen_server:call(?SERVER, {add_or_update, Uri, Document}).

remove_document(Uri) ->
    gen_server:cast(?SERVER, {remove, Uri}).

get_document(Uri) ->
    gen_server:call(?SERVER, {get_document, Uri}).

set_rebar_config(RebarConfig) ->
    gen_server:call(?SERVER, {set_rebar_config, RebarConfig}).

get_rebar_config() ->
    gen_server:call(?SERVER, get_rebar_config).

init(_Args) ->
    error_logger:info_msg("~p:init()", [?MODULE]),
    {ok, #state{dict=dict:new()}}.

handle_call({get_document, Uri}, _From, #state{dict=Dict}=State) ->
    case dict:is_key(Uri, Dict) of
    true -> {reply, {ok, dict:fetch(Uri, Dict)}, State};
    _ -> {reply, not_found, State}
    end;

handle_call({add_or_update, Uri, Document},_From, #state{dict=Dict}=State) ->
    %error_logger:info_msg("add_or_update for ~p", [Uri]),
    NewDict = dict:store(Uri, Document, Dict),
    {reply, ok, State#state{dict=NewDict}};

handle_call({set_rebar_config, RebarConfig}, _From, State) ->
    {reply, ok, State#state{rebar_config = RebarConfig}};

handle_call(get_rebar_config, _From, State) ->
    {reply, State#state.rebar_config, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({remove, Uri}, #state{dict=Dict}=State) ->
    %error_logger:info_msg("remove for ~p", [Uri]),
    NewDict = dict:erase(Uri, Dict),
    {noreply, State#state{dict=NewDict}};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
