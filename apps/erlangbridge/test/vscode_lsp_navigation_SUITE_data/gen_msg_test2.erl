-module(gen_msg_test2).

-export([send_info/1, is_open/0]).

-behaviour(gen_statem).
-export([start/1, stop/1]).
-export([init/1, callback_mode/0, handle_event/4, code_change/4, terminate/3]).

is_open() ->
    gen_statem:call(?MODULE, is_open).

send_info(RoleId) ->
    gen_statem:cast(?MODULE, {send_info, RoleId}).

callback_mode() -> handle_event_function.

start(Args) ->
    gen_statem:start_link(?MODULE, Args, []).
stop(Pid) ->
    gen_statem:stop(Pid).

init(_Args) ->
    {ok, wait, []}.

handle_event(cast, {send_info, _RoleId}, _State, _Date) ->
    keep_state_and_data;

handle_event({call, _From}, is_open, _State, _Date) ->
    keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Date) ->
    keep_state_and_data.

code_change(_Vsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

terminate(_Reason, _State, _Data) ->
    ok.
