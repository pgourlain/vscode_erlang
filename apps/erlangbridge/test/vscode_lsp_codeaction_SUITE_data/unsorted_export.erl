-module(unsorted_export).
-export([c/0, a/1, a/0, b/0]).

a() -> ok.
a(_X) -> ok.
b() -> ok.
c() -> ok.
