-module(record).

-record(rec,
        {a = 1 :: integer(),    % a
         b = 1,                 % b
         c :: integer(),        % c
         d                      % d
         %% after last field
        } % after fields
       ). % end of record definition

-spec f() -> ok.
f() ->
    #rec.a, % a
    A = 1,
    #rec{},
    B = 2,
    #rec{b = "2", % b
         c = 1 % c
        }, % b
    C = 2,
    #rec{a = 1, _ = '_'},
    D = #dummy.a,
    E = 4,
    ok.
