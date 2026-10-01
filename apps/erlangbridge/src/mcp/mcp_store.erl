%% Per-session state of the MCP inspector: the session identity, bounded
%% entity-ID bookkeeping, retained collections (with TTL/count/byte budgets),
%% signed cursors, worker slots and the process-membership evidence.
%%
%% Everything here dies with the session (the process is a child of mcp_sup).
-module(mcp_store).
-behaviour(gen_server).

-export([start_link/2, start_link/3, stop/0]).
-export([session_id/0, mode/0, config/0, started_at/0, now_ms/0]).
-export([entity_id/2, resolve_id/1]).
-export([put_collection/5, fetch_collection/3, make_cursor/4, parse_cursor/3]).
-export([acquire_slot/1, release_slot/1]).
-export([note_member/3, member/1]).
-export([stats/0, notify/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(SWEEP_MS, 1000).
-define(MAX_MEMBERS, 5000).

-record(state, {
          config,
          session_id,
          mode,
          started_at,
          secret,                 %% signs cursors
          max_ids,
          ids_fwd = #{},          %% Key => Id
          ids_rev = #{},          %% Id => Key
          id_order = queue:new(), %% oldest first
          collections = #{},      %% CollId => collection map
          coll_order = [],        %% newest first
          slots = #{},            %% MonitorRef => Pid
          notify,                 %% undefined | fun((map()) -> any()): request journal
          seq = 0,                %% MCP requests seen in this session
          members = #{},          %% Pid => {AppName, ObservedAtMs}
          member_order = queue:new()
         }).

start_link(Config, Mode) ->
    start_link(Config, Mode, undefined).

start_link(Config, Mode, Notify) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Config, Mode, Notify}, []).

stop() ->
    catch gen_server:stop(?SERVER),
    ok.

session_id() -> gen_server:call(?SERVER, session_id).
mode() -> gen_server:call(?SERVER, mode).
config() -> gen_server:call(?SERVER, config).
started_at() -> gen_server:call(?SERVER, started_at).

now_ms() -> erlang:system_time(millisecond).

%% Stable id of an observed entity within the session. A restarted process or
%% a recreated table has a different Key and so a different id.
entity_id(Kind, Key) -> gen_server:call(?SERVER, {entity_id, Kind, Key}).

%% -> {ok, Key} | expired
resolve_id(Id) when is_binary(Id) -> gen_server:call(?SERVER, {resolve_id, Id});
resolve_id(_) -> expired.

%% Meta: map (scope, startedAt, finishedAt, complete, truncated, omissions).
%% Items: [{entity, Map} | {relationship, Map}] in page order.
%% -> {CollId, Meta1, Stored}; items beyond the byte budget are dropped and the
%% collection is marked truncated with a non-resumable limit_reached omission.
put_collection(Tool, ArgsHash, Meta, Items, Ctx) ->
    gen_server:call(?SERVER, {put_collection, Tool, ArgsHash, Meta, Items, Ctx}).

%% -> {ok, Collection} | {error, cursor_expired}
fetch_collection(CollId, Tool, ArgsHash) ->
    gen_server:call(?SERVER, {fetch_collection, CollId, Tool, ArgsHash}).

make_cursor(CollId, Tool, ArgsHash, Offset) ->
    gen_server:call(?SERVER, {make_cursor, CollId, Tool, ArgsHash, Offset}).

%% -> {ok, CollId, Offset} | {error, invalid_cursor}
parse_cursor(Cursor, Tool, ArgsHash) when is_binary(Cursor) ->
    gen_server:call(?SERVER, {parse_cursor, Cursor, Tool, ArgsHash});
parse_cursor(_, _, _) ->
    {error, invalid_cursor}.

%% -> ok | busy. The slot is released when Pid exits or calls release_slot/1.
acquire_slot(Pid) -> gen_server:call(?SERVER, {acquire_slot, Pid}).
release_slot(Pid) -> gen_server:cast(?SERVER, {release_slot, Pid}).

note_member(Pid, App, At) -> gen_server:cast(?SERVER, {note_member, Pid, App, At}).
member(Pid) -> gen_server:call(?SERVER, {member, Pid}).

stats() -> gen_server:call(?SERVER, stats).

%% Journal of MCP requests: numbers the request and hands the (non-secret)
%% metadata map to the notify fun in a separate process, so a slow consumer
%% never delays a request.
notify(Meta) when is_map(Meta) -> gen_server:cast(?SERVER, {notify, Meta}).

%%------------------------------------------------------------------------------

init({Config, Mode, Notify}) ->
    Limits = maps:get(limits, Config),
    Secret = crypto:strong_rand_bytes(32),
    SessionId = <<"s_", (mcp_encoder:hex(crypto:strong_rand_bytes(8)))/binary>>,
    erlang:send_after(?SWEEP_MS, self(), sweep),
    {ok, #state{config = Config, session_id = SessionId, mode = Mode, notify = Notify,
                started_at = now_ms(), secret = Secret,
                max_ids = maps:get(max_items, Limits) * 10}}.

handle_call(session_id, _, S) -> {reply, S#state.session_id, S};
handle_call(mode, _, S) -> {reply, S#state.mode, S};
handle_call(config, _, S) -> {reply, S#state.config, S};
handle_call(started_at, _, S) -> {reply, S#state.started_at, S};

handle_call({entity_id, Kind, Key}, _, S) ->
    {Id, S1} = do_entity_id(Kind, Key, S),
    {reply, Id, S1};
handle_call({resolve_id, Id}, _, S) ->
    {reply, case maps:find(Id, S#state.ids_rev) of
                {ok, Key} -> {ok, Key};
                error -> expired
            end, S};

handle_call({put_collection, Tool, ArgsHash, Meta, Items, Ctx}, _, S) ->
    {CollId, Meta1, Stored, S1} = do_put_collection(Tool, ArgsHash, Meta, Items, Ctx, sweep_collections(S)),
    {reply, {CollId, Meta1, Stored}, S1};
handle_call({fetch_collection, CollId, Tool, ArgsHash}, _, S0) ->
    S = sweep_collections(S0),
    Reply = case maps:find(CollId, S#state.collections) of
                {ok, #{tool := Tool, args_hash := ArgsHash} = C} -> {ok, C};
                _ -> {error, cursor_expired}
            end,
    {reply, Reply, S};

handle_call({make_cursor, CollId, Tool, ArgsHash, Offset}, _, S) ->
    {reply, sign_cursor(CollId, Tool, ArgsHash, Offset, S), S};
handle_call({parse_cursor, Cursor, Tool, ArgsHash}, _, S) ->
    {reply, verify_cursor(Cursor, Tool, ArgsHash, S), S};

handle_call({acquire_slot, Pid}, _, #state{slots = Slots, config = Config} = S) ->
    Max = mcp_policy:limit(max_concurrency, Config),
    case maps:size(Slots) >= Max of
        true -> {reply, busy, S};
        false ->
            Ref = erlang:monitor(process, Pid),
            {reply, ok, S#state{slots = Slots#{Ref => Pid}}}
    end;

handle_call({member, Pid}, _, S) ->
    {reply, maps:get(Pid, S#state.members, undefined), S};

handle_call(stats, _, S) ->
    {reply, #{ids => maps:size(S#state.ids_fwd),
              collections => maps:size(S#state.collections),
              collection_bytes => lists:sum([maps:get(bytes, C) || C <- maps:values(S#state.collections)]),
              slots => maps:size(S#state.slots),
              members => maps:size(S#state.members),
              seq => S#state.seq}, S};

handle_call(_, _, S) -> {reply, {error, unsupported}, S}.

handle_cast({release_slot, Pid}, #state{slots = Slots} = S) ->
    Left = maps:filter(fun(Ref, P) when P =:= Pid -> erlang:demonitor(Ref, [flush]), false;
                          (_, _) -> true
                       end, Slots),
    {noreply, S#state{slots = Left}};
handle_cast({notify, Meta}, #state{notify = Notify, seq = Seq} = S) ->
    N = Seq + 1,
    case Notify of
        F when is_function(F, 1) -> spawn(fun() -> catch F(Meta#{seq => N}) end);
        _ -> ok
    end,
    {noreply, S#state{seq = N}};
handle_cast({note_member, Pid, App, At}, S) ->
    {noreply, do_note_member(Pid, App, At, S)};
handle_cast(_, S) -> {noreply, S}.

handle_info({'DOWN', Ref, process, _, _}, #state{slots = Slots} = S) ->
    {noreply, S#state{slots = maps:remove(Ref, Slots)}};
handle_info(sweep, S) ->
    erlang:send_after(?SWEEP_MS, self(), sweep),
    {noreply, sweep_collections(S)};
handle_info(_, S) -> {noreply, S}.

terminate(_, _) -> ok.

%%------------------------------------------------------------------------------
%% Identities
%%------------------------------------------------------------------------------

do_entity_id(Kind, Key, #state{ids_fwd = Fwd} = S) ->
    case maps:find(Key, Fwd) of
        {ok, Id} -> {Id, S};
        error ->
            Id = <<(prefix(Kind))/binary, (mcp_encoder:hex(crypto:strong_rand_bytes(8)))/binary>>,
            S1 = evict_ids(S#state{ids_fwd = Fwd#{Key => Id},
                                   ids_rev = (S#state.ids_rev)#{Id => Key},
                                   id_order = queue:in(Key, S#state.id_order)}),
            {Id, S1}
    end.

prefix(application) -> <<"a_">>;
prefix(process) -> <<"p_">>;
prefix(module) -> <<"m_">>;
prefix(ets_table) -> <<"t_">>.

%% Bounded bookkeeping: the oldest handles expire (resolve_id/1 -> expired).
evict_ids(#state{ids_fwd = Fwd, max_ids = Max} = S) when map_size(Fwd) =< Max -> S;
evict_ids(#state{id_order = Q, ids_fwd = Fwd, ids_rev = Rev} = S) ->
    case queue:out(Q) of
        {{value, Key}, Q1} ->
            Id = maps:get(Key, Fwd),
            evict_ids(S#state{id_order = Q1, ids_fwd = maps:remove(Key, Fwd), ids_rev = maps:remove(Id, Rev)});
        {empty, _} -> S
    end.

do_note_member(Pid, App, At, #state{members = M, member_order = Q} = S) ->
    case maps:is_key(Pid, M) of
        true -> S#state{members = M#{Pid => {App, At}}};
        false ->
            S1 = S#state{members = M#{Pid => {App, At}}, member_order = queue:in(Pid, Q)},
            trim_members(S1)
    end.

trim_members(#state{members = M} = S) when map_size(M) =< ?MAX_MEMBERS -> S;
trim_members(#state{members = M, member_order = Q} = S) ->
    case queue:out(Q) of
        {{value, Pid}, Q1} -> trim_members(S#state{members = maps:remove(Pid, M), member_order = Q1});
        {empty, _} -> S
    end.

%%------------------------------------------------------------------------------
%% Collections
%%------------------------------------------------------------------------------

do_put_collection(Tool, ArgsHash, Meta, Items, Ctx, #state{config = Config} = S) ->
    MaxBytes = mcp_policy:limit(max_collection_bytes, Config),
    MaxColls = mcp_policy:limit(max_collections, Config),
    {Stored, Dropped} = fit(Items, MaxBytes, 0, []),
    CollId = <<"c_", (mcp_encoder:hex(crypto:strong_rand_bytes(6)))/binary>>,
    Meta1 = case Dropped of
                0 -> Meta;
                _ -> add_limit_omission(Meta, Dropped)
            end,
    Bytes = erlang:external_size(Stored),
    Coll = #{tool => Tool, args_hash => ArgsHash, meta => Meta1, items => Stored,
             bytes => Bytes, created => erlang:monotonic_time(millisecond), ctx => Ctx},
    Colls = (S#state.collections)#{CollId => Coll},
    S1 = evict_collections(S#state{collections = Colls, coll_order = [CollId | S#state.coll_order]}, MaxColls),
    {CollId, Meta1, Stored, S1}.

fit([], _, _, Acc) -> {lists:reverse(Acc), 0};
fit([I | T] = All, Max, Used, Acc) ->
    Size = erlang:external_size(I),
    case Used + Size > Max of
        true -> {lists:reverse(Acc), length(All)};
        false -> fit(T, Max, Used + Size, [I | Acc])
    end.

add_limit_omission(Meta, Dropped) ->
    Om = #{<<"reason">> => <<"limit_reached">>,
           <<"count">> => Dropped,
           <<"resumable">> => false,
           <<"message">> => <<"collection byte budget reached; narrow the scope (application, subtree or depth)">>},
    Meta#{omissions => maps:get(omissions, Meta, []) ++ [Om], truncated => true, complete => false}.

evict_collections(#state{coll_order = Order, collections = Colls} = S, Max) ->
    {Keep, Drop} = case length(Order) > Max of
                       true -> lists:split(Max, Order);
                       false -> {Order, []}
                   end,
    S#state{coll_order = Keep, collections = maps:without(Drop, Colls)}.

sweep_collections(#state{collections = Colls, config = Config} = S) ->
    Ttl = mcp_policy:limit(collection_ttl_ms, Config),
    Now = erlang:monotonic_time(millisecond),
    Alive = maps:filter(fun(_, #{created := C}) -> Now - C < Ttl end, Colls),
    S#state{collections = Alive,
            coll_order = [Id || Id <- S#state.coll_order, maps:is_key(Id, Alive)]}.

%%------------------------------------------------------------------------------
%% Cursors: c1.<collection>.<offset>.<signature>, bound to session, collection,
%% tool and arguments.
%%------------------------------------------------------------------------------

sign_cursor(CollId, Tool, ArgsHash, Offset, S) ->
    Sig = signature(CollId, Tool, ArgsHash, Offset, S),
    <<"c1.", CollId/binary, ".", (integer_to_binary(Offset))/binary, ".", Sig/binary>>.

signature(CollId, Tool, ArgsHash, Offset, #state{secret = Secret, session_id = Sid}) ->
    Data = [Secret, Sid, 0, CollId, 0, Tool, 0, ArgsHash, 0, integer_to_binary(Offset)],
    <<Head:12/binary, _/binary>> = crypto:hash(sha256, Data),
    mcp_encoder:hex(Head).

verify_cursor(Cursor, Tool, ArgsHash, S) ->
    case binary:split(Cursor, <<".">>, [global]) of
        [<<"c1">>, CollId, OffBin, Sig] when byte_size(CollId) < 64, byte_size(OffBin) < 12 ->
            try binary_to_integer(OffBin) of
                Offset when Offset >= 0 ->
                    case constant_eq(Sig, signature(CollId, Tool, ArgsHash, Offset, S)) of
                        true -> {ok, CollId, Offset};
                        false -> {error, invalid_cursor}
                    end;
                _ -> {error, invalid_cursor}
            catch _:_ -> {error, invalid_cursor}
            end;
        _ ->
            {error, invalid_cursor}
    end.

constant_eq(A, B) when byte_size(A) =:= byte_size(B) ->
    fold_xor(binary_to_list(A), binary_to_list(B), 0) =:= 0;
constant_eq(_, _) ->
    false.

fold_xor([], [], Acc) -> Acc;
fold_xor([X | Xs], [Y | Ys], Acc) -> fold_xor(Xs, Ys, Acc bor (X bxor Y)).
