-module(gen_lsp_doc_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_or_update_document/2, remove_document/1]).

-define(SERVER, ?MODULE).

-record(state, {dict}).

start_link() ->
    error_logger:info_msg("~p:start_link()", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

add_or_update_document(Uri, Document) -> 
    gen_server:cast(?SERVER, {add_or_update, Uri, Document}).

remove_document(Uri) ->
    gen_server:cast(?SERVER, {remove, Uri}).


init(_Args) ->
    error_logger:info_msg("~p:init()", [?MODULE]),
    {ok, #state{dict=dict:new()}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_or_update, Uri, Document}, #state{dict=Dict}) ->
    %error_logger:info_msg("add_or_update for ~p", [Uri]),
    NewDict = dict:store(Uri, Document, Dict),
    {noreply, #state{dict=NewDict}};

handle_cast({remove, Uri}, #state{dict=Dict}) ->
    %error_logger:info_msg("remove for ~p", [Uri]),
    NewDict = dict:erase(Uri, Dict),
    {noreply, #state{dict=NewDict}};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, StateData) ->
    {noreply, StateData}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
