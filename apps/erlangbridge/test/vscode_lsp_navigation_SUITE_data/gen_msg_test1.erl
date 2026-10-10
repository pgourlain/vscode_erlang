-module(gen_msg_test1).

-export([get_val/1, logout/1]).

-behaviour(gen_server).
-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

get_val(DropId) ->
    gen_server:call(?MODULE, {get_val, DropId}).

logout(Args) ->
    gen_server:cast(?MODULE, {logout, Args}).

stop(_Name) ->
    gen_server:call(?MODULE, stop).

start_link(_Name) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, []}.


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({get_val, _DropId}, _From, State) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({logout, _Args}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
