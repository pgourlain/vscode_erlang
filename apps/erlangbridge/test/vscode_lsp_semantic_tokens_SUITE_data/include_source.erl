-module(include_source).
-include("tokens_header.hrl").

f(R) ->
    R#header_rec.x.
