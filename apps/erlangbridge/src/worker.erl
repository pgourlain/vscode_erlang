-module(worker).
-export([start/2]).

-define(KILL_AFTER, 10 * 60000).

start(Fun, ID) ->
    Owner = self(),
    spawn_link(fun () ->
        {Worker, Monitor} = spawn_monitor(fun () ->
            try
                erlang:send(Owner, {worker_result, ID, Fun()})
            catch _Class:Exception:Stack ->
                logger:error("~p", [Stack]),
                erlang:send(Owner, {worker_error, ID, Exception})
            end
        end),
        receive
            {'DOWN', Monitor, process, Worker, _Reason} -> ok
        after
            ?KILL_AFTER ->
                logger:error("Worker ~p killed due to timeout", [Worker]),
                exit(Worker, kill),
                erlang:send(Owner, {worker_error, ID, timeout})
        end
    end).