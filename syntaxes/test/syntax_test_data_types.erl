-module(syntax_test_data_types).

-compile(export_all).

-spec ascii_numbers() -> ok.
ascii_numbers() ->
    $ ,
    $!, $", $#, $$, $%, $&, $', $(, $), $*, $+, $,, $-, $., $/,
    $0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
    $:, $;, $<, $=, $>, $?, $@,
    $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
    $[, $\\, $], $^, $_, $`,
    $a, $b, $c,$d, $e, $f, $g, $h, $i, $j,  $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
    ${, $|,  $}, $~,
    ok.

-spec integers() -> ok.
integers() ->
    42, -1, +1,
    -1_234_567_890,
    2#101, 2#1010_1010,
    8#765, 8#765_432,
    10#123, 10#123_456,
    16#1f2E, 16#4865_316F_774F_6C64,
    36#1234567890abcdefghijklmnopqrstuvwxyz, 36#1234567890ABC_DEF_GHIJ_KLMNOP_QR_STUVWXYZ,
    ok.

-spec floats() -> ok.
floats() ->
    2.3, 2.3e3, 2.3e-3,
    1_234.333_333,
    ok.

-spec atoms() -> ok.
atoms() ->
    hello, phone_number, 'Monday', 'phone number', bob123@best_place,
    %% ASCII characters in single quoted atom. Note: the escaped single quote (\') is not in the right position ...
    ' !"#$%&()*+,-./0123456789:;\'<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~',
    %% ... and the reason is that if the escaped single quote followed by an opening parenthesis then it's falsely
    %% interpreted as a function call
    'atom\'(A)', %' % TODO: escaped single quote and opening parenthesis ("\'(") shall not break atoms
    a, 'atom\'(A)',  %' % TODO: -"-
    ok.

-spec bitstrings_and_binaries() -> ok.
bitstrings_and_binaries() ->
    <<10,20>>,
    <<1:1, 0:1>>,
    <<$a, $b, $c>>,
    <<42/integer, 42:16/integer>>,
    <<2.3/float, 2.3e3:32/float>>,
    <<"ABC", "ABC"/utf8, "ABC"/utf16, "ABC"/utf32>>,
    <<$a/utf8, 98/utf16, $c/utf32, 1024/utf8>>,
    <<2.3/unsigned-big-float>>,
    <<42:16/signed-little-unit:2-integer>>,
    <<1:1/binary>>,
    <<2:2/bytes>>,
    <<3:3/bitstring>>,
    <<4:4/bits>>,

    _Bin1 = <<1,17,42>>,
    _Bin2 = <<"abc">>,
    _Bin3 = <<1,17,42:16>>,
    <<_A,_B,_C:16>> = <<1,17,42:16>>,
    <<_D:16,_E,_F>> = <<1,17,42:16>>,
    <<G,_H/binary>> = <<1,17,42:16>>,
    <<G,_J/bitstring>> = <<1,17,42:12>>,
    ok.

-spec explicit_function_expressions1() -> ok.
explicit_function_expressions1() ->
    fun(0) -> 1; (A) -> A+1 end,
    ok.

-spec explicit_function_expressions2() -> ok.
explicit_function_expressions2() ->
    fun (A) when 0/=A -> 1/A;
        (_) -> 1
    end,
    ok.

-spec explicit_function_expressions3() -> ok.
explicit_function_expressions3() ->
    fun
        (A) when 0/=A ->
            1/A;
        (_) ->
            1
    end,
    ok.

-spec explicit_function_expressions4() -> ok.
explicit_function_expressions4() ->
    fun Fact(1) -> 1; % TODO: Name 'Fact' breaks syntax highlight. See '-define' below.
        Fact(N) -> N * Fact(N-1)
    end,
    ok.

-spec explicit_function_expressions5() -> ok.
explicit_function_expressions5() ->
    fun
        Fact(1) -> % TODO: Name 'Fact' breaks syntax highlight. See '-define' below.
            1;
        Fact(N) ->
            N * Fact(N-1)
    end,
    ok.

-define(PLUS_ONE, plus_one).
-define(ARITY, 1).
-spec implicit_function_expressions() -> ok.
implicit_function_expressions() ->
    %% In Name/Arity, Name is an atom and Arity is an integer.
    lists:map(fun plus_one/1, [1, 2, 3]),
    lists:map(fun plus_one/?ARITY, [1, 2, 3]),
    lists:map(fun ?PLUS_ONE/1, [1, 2, 3]),
    lists:map(fun ?PLUS_ONE/?ARITY, [1, 2, 3]),

    %% In Module:Name/Arity, Module, and Name are atoms and Arity is an integer.
    %% Starting from Erlang/OTP R15, Module, Name, and Arity can also be variables.
    Mod = ?MODULE,
    Fun = plus_one,
    Arity = 1,
    lists:map(fun syntax_test_data_types:plus_one/1, [1, 2, 3]),
    lists:map(fun syntax_test_data_types:plus_one/Arity, [1, 2, 3]),
    lists:map(fun syntax_test_data_types:plus_one/?ARITY, [1, 2, 3]),
    lists:map(fun syntax_test_data_types:Fun/1, [1, 2, 3]),
    lists:map(fun syntax_test_data_types:Fun/Arity, [1, 2, 3]),
    lists:map(fun syntax_test_data_types:Fun/?ARITY, [1, 2, 3]),
    lists:map(fun syntax_test_data_types:?PLUS_ONE/1, [1, 2, 3]),
    lists:map(fun syntax_test_data_types:?PLUS_ONE/Arity, [1, 2, 3]),
    lists:map(fun syntax_test_data_types:?PLUS_ONE/?ARITY, [1, 2, 3]),

    lists:map(fun Mod:plus_one/1, [1, 2, 3]),
    lists:map(fun Mod:plus_one/Arity, [1, 2, 3]),
    lists:map(fun Mod:plus_one/?ARITY, [1, 2, 3]),
    lists:map(fun Mod:Fun/1, [1, 2, 3]),
    lists:map(fun Mod:Fun/Arity, [1, 2, 3]),
    lists:map(fun Mod:Fun/?ARITY, [1, 2, 3]),
    lists:map(fun Mod:?PLUS_ONE/1, [1, 2, 3]),
    lists:map(fun Mod:?PLUS_ONE/Arity, [1, 2, 3]),
    lists:map(fun Mod:?PLUS_ONE/?ARITY, [1, 2, 3]),

    lists:map(fun ?MODULE:plus_one/1, [1, 2, 3]), % TODO: syntax highlight function name
    lists:map(fun ?MODULE:plus_one/Arity, [1, 2, 3]), % TODO: syntax highlight function name
    lists:map(fun ?MODULE:plus_one/?ARITY, [1, 2, 3]), % TODO: syntax highlight function name
    lists:map(fun ?MODULE:Fun/1, [1, 2, 3]),
    lists:map(fun ?MODULE:Fun/Arity, [1, 2, 3]),
    lists:map(fun ?MODULE:Fun/?ARITY, [1, 2, 3]),
    lists:map(fun ?MODULE:?PLUS_ONE/1, [1, 2, 3]),
    lists:map(fun ?MODULE:?PLUS_ONE/Arity, [1, 2, 3]),
    lists:map(fun ?MODULE:?PLUS_ONE/?ARITY, [1, 2, 3]),
    ok.

-spec plus_one(N::integer()) -> ok.
plus_one(N) ->
    N + 1.

-spec 'Plus One'(N::integer()) -> ok.
'Plus One'(N) ->
    N + 1.

-spec 'Plus \'()ne'(N::integer()) -> ok. %' % TODO: escaped single quote and opening parenthesis ("\'(") shall not break atoms
'Plus \'()ne'(N) ->
    N + 1. %'. % TODO: escaped single quote and opening parenthesis ("\'(") shall not break atoms

-spec tuples() -> ok.
tuples() ->
    {adam, 24, {july, 29}},
    ok.

-spec maps() -> ok.
maps() ->
    #{},
    #{name => adam, age => 24, date => {july, 29}},
    ok.

-spec lists() -> ok.
lists() ->
    [a, 2, {c, 4} | []],
    "string" "42",
    ok.

-record(rec,
        {a = 1 :: integer(),    % a
         b = 1,                 % b
         c :: integer(),        % c
         d                      % d
         %% after last field
        } % after fields
       ). % end of record definition

-spec records() -> ok.
records() ->
    #rec.a, % a
    _A = 1,
    #rec{},
    _B = 2,
    #rec{b = "2", % b
         c = 1 % c
        }, % b
    _C = 2,
    #rec{a = 1, _ = '_'},
    _E = 4,
    ok.

-spec booleans() -> ok.
booleans() ->
    true or false.

-spec other_language_constants() -> ok.
other_language_constants() ->
    undefined,
    ok.

-spec escape_sequences() -> ok.
escape_sequences() ->
    "a\bc\d\e\fghijklm\nopq\r\s\tu\vwxyz",
    "\^a\^b\^c\^d\^e\^f\^g\^h\^i\^j\^k\^l\^m\^n\^o\^p\^q\^r\^s\^t\^u\^v\^w\^x\^y\^z",
    "\^A\^B\^C\^D\^E\^F\^G\^H\^I\^J\^K\^L\^M\^N\^O\^P\^Q\^R\^S\^T\^U\^V\^W\^X\^Y\^Z",
    "a\"b\'c\\d",
    "n\157p",
    "j\x6bl\x6Dn",

    'a\bc\d\e\fghijklm\nopq\r\s\tu\vwxyz',
    '\^a\^b\^c\^d\^e\^f\^g\^h\^i\^j\^k\^l\^m\^n\^o\^p\^q\^r\^s\^t\^u\^v\^w\^x\^y\^z',
    '\^A\^B\^C\^D\^E\^F\^G\^H\^I\^J\^K\^L\^M\^N\^O\^P\^Q\^R\^S\^T\^U\^V\^W\^X\^Y\^Z',
    'a\"b\'c\\d',
    'n\157p',
    'j\x6bl\x6Dn',

    $ ,
    $\b, $\d, $\e, $\f, $\n, $\r, $\s, $\t, $\v,
    $\^a, $\^b, $\^c, $\^d, $\^e, $\^f, $\^g, $\^h, $\^i, $\^j, $\^k, $\^l, $\^m,
    $\^n, $\^o, $\^p, $\^q, $\^r, $\^s, $\^t, $\^u, $\^v, $\^w, $\^x, $\^y, $\^z,
    $\^A, $\^B, $\^C, $\^D, $\^E, $\^F, $\^G, $\^H, $\^I, $\^J, $\^K, $\^L, $\^M,
    $\^N, $\^O, $\^P, $\^Q, $\^R, $\^S, $\^T, $\^U, $\^V, $\^W, $\^X, $\^Y, $\^Z,
    $\", $\',
    $\157,
    $\x6b, $\x6D,
    ok.

-define(THE_END, ok).
