-module(lsp_completion).

-export([module_function/2]).

module_function(Module, Prefix) ->
    functions_in_standard_module(Module, Prefix).

functions_in_standard_module(Module, Prefix) ->
    case code:ensure_loaded(Module) of
        {module, _} ->
            Exports = [atom_to_list(element(1, Export)) || Export <- proplists:get_value(exports, Module:module_info())],
            #{items => lists:filtermap(fun (Name) ->
                case lists:prefix(Prefix, Name) of
                    true -> {true, list_to_binary(Name)};
                    _ -> false
                end
            end, sets:to_list(sets:from_list(Exports)))};
        _ ->
            #{error => <<"No such module">>}
    end.
