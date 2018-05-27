-module(gen_lsp_doc_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_or_update_document/2, remove_document/1, get_document/1, get_documents/0, set_config/1, get_config/0]).

-define(SERVER, ?MODULE).

-record(state, {dict, config}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

add_or_update_document(Uri, Document) -> 
    gen_server:call(?SERVER, {add_or_update, Uri, Document}).

remove_document(Uri) ->
    gen_server:cast(?SERVER, {remove, Uri}).

get_document(Uri) ->
    gen_server:call(?SERVER, {get_document, Uri}).

get_documents() ->
    gen_server:call(?SERVER, get_documents).

set_config(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

get_config() ->
    gen_server:call(?SERVER, get_config).

init(_Args) ->
    {ok, #state{dict=dict:new(), config=#{}}}.

handle_call({get_document, Uri}, _From, #state{dict=Dict}=State) ->
    case dict:is_key(Uri, Dict) of
    true -> {reply, {ok, dict:fetch(Uri, Dict)}, State};
    _ -> {reply, not_found, State}
    end;

handle_call({add_or_update, Uri, Document},_From, #state{dict=Dict}=State) ->
    NewDict = dict:store(Uri, Document, Dict),
    {reply, ok, State#state{dict=NewDict}};

handle_call(get_documents, _From, State) ->
    {reply, dict:fetch_keys(State#state.dict), State};

handle_call({set_config, Config}, _From, State) ->
    {reply, #{}, State#state{config = Config}};

handle_call(get_config, _From, State) ->
    {reply, State#state.config, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({remove, Uri}, #state{dict=Dict}=State) ->
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
