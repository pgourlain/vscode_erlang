-module(function_type_spec).

-type fun_type() :: fun((term()) -> ok).

-spec f() -> fun_type().
f() ->
    fun(_) -> ok end.
