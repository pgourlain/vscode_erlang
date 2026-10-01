%% Bounded conversion of Erlang terms to JSON-encodable terms (binary keys,
%% binaries, numbers, booleans, null, lists, maps). Never creates atoms, never
%% exposes an executable handle: funs, ports, references and pids become text.
-module(mcp_encoder).

-export([term/2, text/2, safe_binary/2, hex/1, iso8601/1, pid_text/1]).

-define(SECRET_WORDS, ["password", "passwd", "secret", "token", "cookie",
                       "credential", "apikey", "api_key", "private_key", "auth"]).

%% Limits: #{max_depth, max_items, max_binary_bytes}
term(Term, Limits) ->
    enc(Term, 0, Limits).

%% Short printable text of any term, bounded.
text(Term, Limits) ->
    Max = maps:get(max_binary_bytes, Limits, 4096),
    Depth = maps:get(max_depth, Limits, 16),
    Str = try lists:flatten(io_lib:format("~0P", [Term, Depth]))
          catch _:_ -> "<unprintable>"
          end,
    safe_binary(unicode:characters_to_binary(Str, unicode, utf8), Max).

enc(_, Depth, #{max_depth := Max}) when Depth > Max ->
    <<"<max depth>">>;
enc(T, _, _) when is_integer(T) -> T;
enc(T, _, _) when is_float(T) -> T;
enc(true, _, _) -> true;
enc(false, _, _) -> false;
enc(undefined, _, _) -> null;
enc(T, _, _) when is_atom(T) ->
    safe_binary(atom_to_binary(T, utf8), 255);
enc(T, _, #{max_binary_bytes := Max}) when is_binary(T) ->
    binary(T, Max);
enc(T, _, _) when is_pid(T) -> pid_text(T);
enc(T, _, _) when is_port(T) -> #{<<"port">> => list_bin(erlang:port_to_list(T))};
enc(T, _, _) when is_reference(T) -> #{<<"ref">> => list_bin(erlang:ref_to_list(T))};
enc(T, _, _) when is_function(T) ->
    %% Descriptive only. The fun itself is never reachable from the result.
    case erlang:fun_info(T, module) of
        {module, M} -> #{<<"fun">> => safe_binary(atom_to_binary(M, utf8), 255)};
        _ -> #{<<"fun">> => <<"unknown">>}
    end;
enc(T, Depth, Limits) when is_tuple(T) ->
    #{<<"tuple">> => enc_list(tuple_to_list(T), Depth + 1, Limits)};
enc(T, Depth, Limits) when is_map(T) ->
    Max = maps:get(max_items, Limits),
    Pairs = lists:sublist(lists:sort(maps:to_list(T)), Max),
    #{<<"map">> => [#{<<"key">> => enc(K, Depth + 1, Limits),
                      <<"value">> => enc_value(K, V, Depth + 1, Limits)} || {K, V} <- Pairs],
      <<"size">> => map_size(T)};
enc([], _, _) -> [];
enc(T, Depth, Limits) when is_list(T) ->
    case printable(T) of
        true -> safe_binary(unicode:characters_to_binary(T, unicode, utf8), maps:get(max_binary_bytes, Limits));
        false -> enc_list(T, Depth + 1, Limits)
    end;
enc(_, _, _) ->
    <<"<unsupported>">>.

enc_value(Key, Value, Depth, Limits) ->
    case secret_key(Key) of
        true -> <<"<redacted>">>;
        false -> enc(Value, Depth, Limits)
    end.

secret_key(Key) when is_atom(Key) -> secret_key(atom_to_list(Key));
secret_key(Key) when is_binary(Key) ->
    secret_key(binary_to_list(Key));
secret_key(Key) when is_list(Key) ->
    try
        Lower = string:to_lower(Key),
        lists:any(fun(W) -> string:str(Lower, W) > 0 end, ?SECRET_WORDS)
    catch _:_ -> false
    end;
secret_key(_) -> false.

%% Proper or improper list, capped by max_items.
enc_list(L, Depth, #{max_items := Max} = Limits) ->
    {Items, Rest} = take(L, Max, []),
    Encoded = [enc(I, Depth, Limits) || I <- Items],
    case Rest of
        [] -> Encoded;
        R when is_list(R) -> Encoded ++ [#{<<"truncated">> => true}];
        Tail -> Encoded ++ [#{<<"improperTail">> => enc(Tail, Depth, Limits)}]
    end.

take([H | T], N, Acc) when N > 0 -> take(T, N - 1, [H | Acc]);
take(Rest, _, Acc) -> {lists:reverse(Acc), Rest}.

printable(L) ->
    try io_lib:printable_unicode_list(L) catch _:_ -> false end.

binary(B, Max) ->
    case is_utf8(B) andalso byte_size(B) =< Max of
        true -> B;
        false ->
            Size = byte_size(B),
            Head = binary:part(B, 0, min(Size, Max)),
            #{<<"binary">> => #{<<"size">> => Size,
                                <<"encoding">> => <<"base64">>,
                                <<"data">> => base64:encode(Head),
                                <<"truncated">> => Size > Max}}
    end.

%% A valid UTF-8 binary cut at Max bytes, on a character boundary.
safe_binary(B, Max) when is_binary(B) ->
    case is_utf8(B) of
        true when byte_size(B) =< Max -> B;
        true -> cut_utf8(B, Max);
        false -> <<"<invalid utf-8>">>
    end.

cut_utf8(B, Max) ->
    Head = binary:part(B, 0, Max),
    case unicode:characters_to_binary(Head, utf8, utf8) of
        R when is_binary(R) -> R;
        {incomplete, Good, _} -> Good;
        _ -> <<>>
    end.

is_utf8(B) ->
    case unicode:characters_to_binary(B, utf8, utf8) of
        R when is_binary(R) -> true;
        _ -> false
    end.

list_bin(L) -> list_to_binary(L).

pid_text(Pid) -> list_to_binary(pid_to_list(Pid)).

hex(Bin) ->
    << <<(hex_digit(N div 16)), (hex_digit(N rem 16))>> || <<N>> <= Bin >>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.

%% Unix milliseconds -> "2026-01-01T00:00:00.000Z"
iso8601(Ms) ->
    Secs = Ms div 1000,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(
                                 Secs + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})),
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0wZ",
                                   [Y, Mo, D, H, Mi, S, Ms rem 1000])).
