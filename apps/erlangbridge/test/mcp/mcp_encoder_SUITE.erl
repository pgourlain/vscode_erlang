-module(mcp_encoder_SUITE).

%% Bounded, safe conversion of arbitrary terms (mcp_encoder).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [scalars_and_special_terms,
     binaries_are_validated_and_bounded,
     depth_and_item_limits,
     maps_with_non_string_keys_and_secret_keys,
     improper_lists_and_unicode,
     never_creates_atoms_and_always_json_encodable,
     text_is_bounded,
     credentials_are_redacted_in_terms_text_and_urls,
     timestamps_and_hex].

credentials_are_redacted_in_terms_text_and_urls(_) ->
    L = #{max_depth => 8, max_items => 20, max_binary_bytes => 256},
    %% key/value pairs (proplists, options) and map entries
    ?assertEqual(#{<<"tuple">> => [<<"password">>, <<"<redacted>">>]}, mcp_encoder:term({password, "hunter2"}, L)),
    ?assertEqual(#{<<"tuple">> => [<<"db">>, <<"ok">>]}, mcp_encoder:term({db, ok}, L)),
    %% printed form (child ids): same rule, nested, and URLs with credentials
    T = mcp_encoder:text({conn, [{host, "h"}, {api_key, "SENTINEL_KEY"}, {opts, #{secret => "SENTINEL_S"}}]}, L),
    ?assertEqual(nomatch, binary:match(T, <<"SENTINEL">>)),
    ?assertNotEqual(nomatch, binary:match(T, <<"redacted">>)),
    ?assertNotEqual(nomatch, binary:match(T, <<"host">>)),
    ?assertEqual(<<"postgres://<redacted>@db/app">>, mcp_encoder:scrub(<<"postgres://user:pw@db/app">>)),
    ?assertEqual(<<"http://example.org/a:b">>, mcp_encoder:scrub(<<"http://example.org/a:b">>)),
    ?assertEqual(<<"see <redacted> now">>,
                 binary:replace(mcp_encoder:term(<<"see https://u:p@h now">>, L), <<"https://<redacted>@h">>, <<"<redacted>">>)),
    ?assertEqual(<<"db: <redacted>">>, binary:replace(mcp_encoder:term("db: ftp://a:b@c", L), <<"ftp://<redacted>@c">>, <<"<redacted>">>)).

limits() -> #{max_depth => 4, max_items => 5, max_binary_bytes => 16}.

scalars_and_special_terms(_) ->
    L = limits(),
    ?assertEqual(42, mcp_encoder:term(42, L)),
    ?assertEqual(1.5, mcp_encoder:term(1.5, L)),
    ?assertEqual(true, mcp_encoder:term(true, L)),
    ?assertEqual(null, mcp_encoder:term(undefined, L)),
    ?assertEqual(<<"hello">>, mcp_encoder:term(hello, L)),
    ?assertEqual([], mcp_encoder:term([], L)),
    ?assertEqual(<<"<0.1.0>">>, mcp_encoder:term(list_to_pid("<0.1.0>"), L)),
    ?assertMatch(#{<<"port">> := _}, mcp_encoder:term(hd(erlang:ports()), L)),
    ?assertMatch(#{<<"ref">> := _}, mcp_encoder:term(make_ref(), L)),
    %% functions are described, never a recoverable handle
    Fun = mcp_encoder:term(fun lists:sort/1, L),
    ?assertEqual(#{<<"fun">> => <<"lists">>}, Fun),
    ?assertEqual(#{<<"fun">> => <<"mcp_encoder_SUITE">>}, mcp_encoder:term(fun() -> secret_closure end, L)),
    ?assertEqual(nomatch, re:run(io_lib:format("~p", [Fun]), "sort")),
    ?assertEqual(#{<<"tuple">> => [1, <<"a">>]}, mcp_encoder:term({1, a}, L)).

binaries_are_validated_and_bounded(_) ->
    L = limits(),
    ?assertEqual(<<"short">>, mcp_encoder:term(<<"short">>, L)),
    Long = binary:copy(<<"x">>, 100),
    ?assertMatch(#{<<"binary">> := #{<<"size">> := 100, <<"truncated">> := true, <<"encoding">> := <<"base64">>}},
                 mcp_encoder:term(Long, L)),
    Bin = mcp_encoder:term(<<255, 254, 1, 2>>, L),
    ?assertMatch(#{<<"binary">> := #{<<"size">> := 4, <<"truncated">> := false}}, Bin),
    ?assertEqual(<<255, 254, 1, 2>>, base64:decode(maps:get(<<"data">>, maps:get(<<"binary">>, Bin)))),
    %% base64 payload is bounded by max_binary_bytes, never the whole binary
    Big = mcp_encoder:term(binary:copy(<<0>>, 1000000), L),
    ?assert(byte_size(maps:get(<<"data">>, maps:get(<<"binary">>, Big))) =< 24),
    %% safe_binary cuts on a character boundary
    ?assertEqual(<<"aé"/utf8>>, mcp_encoder:safe_binary(<<"aéb"/utf8>>, 3)),
    ?assertEqual(<<"a"/utf8>>, mcp_encoder:safe_binary(<<"aé"/utf8>>, 2)),
    ?assertEqual(<<"<invalid utf-8>">>, mcp_encoder:safe_binary(<<255>>, 10)).

depth_and_item_limits(_) ->
    L = limits(),
    Deep = lists:foldl(fun(_, Acc) -> [Acc] end, x, lists:seq(1, 1000)),
    Enc = mcp_encoder:term(Deep, L),
    ?assertNotEqual(nomatch, re:run(io_lib:format("~p", [Enc]), "max depth")),
    ?assert(length(lists:flatten(io_lib:format("~p", [Enc]))) < 400),
    Many = mcp_encoder:term(lists:seq(1, 1000), L),
    ?assertEqual([1, 2, 3, 4, 5, #{<<"truncated">> => true}], Many),
    Tuple = mcp_encoder:term(list_to_tuple(lists:seq(1, 1000)), L),
    ?assertEqual(6, length(maps:get(<<"tuple">>, Tuple))).

maps_with_non_string_keys_and_secret_keys(_) ->
    L = limits(),
    M = mcp_encoder:term(#{1 => a, {t, 1} => b, <<"k">> => c, "str" => d}, L),
    ?assertEqual(4, maps:get(<<"size">>, M)),
    Keys = [maps:get(<<"key">>, E) || E <- maps:get(<<"map">>, M)],
    ?assert(lists:member(1, Keys)),
    ?assert(lists:member(#{<<"tuple">> => [<<"t">>, 1]}, Keys)),
    %% likely secrets are redacted by key name
    S = mcp_encoder:term(#{password => "SENTINEL_PW", <<"api_key">> => <<"SENTINEL_KEY">>,
                           auth_token => x, cookie => y, name => visible}, L),
    Text = iolist_to_binary(io_lib:format("~p", [S])),
    ?assertEqual(nomatch, binary:match(Text, <<"SENTINEL">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"visible">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"<redacted>">>)),
    Many = mcp_encoder:term(maps:from_list([{I, I} || I <- lists:seq(1, 100)]), L),
    ?assertEqual(5, length(maps:get(<<"map">>, Many))),
    ?assertEqual(100, maps:get(<<"size">>, Many)).

improper_lists_and_unicode(_) ->
    L = limits(),
    ?assertEqual([1, 2, #{<<"improperTail">> => <<"tail">>}], mcp_encoder:term([1, 2 | tail], L)),
    ?assertEqual(<<"héllo"/utf8>>, mcp_encoder:term("héllo", L#{max_binary_bytes => 64})),
    ?assertEqual(<<"日本語"/utf8>>, mcp_encoder:term([26085, 26412, 35486], L#{max_binary_bytes => 64})),
    %% invalid code points fall back to a bounded list instead of crashing
    ?assert(is_list(mcp_encoder:term([1114112, 2], L)) orelse is_binary(mcp_encoder:term([1114112, 2], L))).

never_creates_atoms_and_always_json_encodable(_) ->
    L = limits(),
    Terms = [self(), make_ref(), fun() -> ok end, {a, {b, {c, {d, {e, f}}}}}, #{a => #{b => #{c => #{d => #{e => f}}}}},
             [a | b], <<0, 255>>, "hi", 'weird atom', 1 bsl 200, -0.0, "", <<>>, {}, #{}],
    Before = erlang:system_info(atom_count),
    [begin
         J = mcp_encoder:term(T, L),
         {ok, Json} = vscode_jsone:encode(J),
         ?assert(is_binary(iolist_to_binary(Json)))
     end || T <- Terms],
    [mcp_encoder:term(list_to_binary("unique_" ++ integer_to_list(I)), L) || I <- lists:seq(1, 500)],
    ?assertEqual(Before, erlang:system_info(atom_count)).

text_is_bounded(_) ->
    L = limits(),
    T = mcp_encoder:text(lists:seq(1, 100000), L),
    ?assert(byte_size(T) =< 16),
    ?assertEqual(<<"worker_a">>, mcp_encoder:text(worker_a, L#{max_binary_bytes => 64})),
    ?assert(is_binary(mcp_encoder:text(make_ref(), L#{max_binary_bytes => 64}))).

timestamps_and_hex(_) ->
    ?assertEqual(<<"1970-01-01T00:00:00.000Z">>, mcp_encoder:iso8601(0)),
    ?assertEqual(<<"2026-09-29T12:34:56.789Z">>, mcp_encoder:iso8601(1790685296789)),
    ?assertEqual(<<"00ff10">>, mcp_encoder:hex(<<0, 255, 16>>)).
