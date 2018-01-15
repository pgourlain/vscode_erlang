-module(vscode_lsp_entry).


-export([start/0, start/1]).


start() ->
    io:format("vscode_lsp_entry started/0"),
    init_lsp().

start(_Args) ->
    io:format("vscode_lsp_entry started/1(~p)", [_Args]),
    init_lsp().

get_port() ->
    case init:get_argument(vscode_port) of
    {ok, [[P]]} -> P;
    _ -> 0
    end.

init_lsp() ->
    case  compile_needed_modules() of
    ok ->
        case application:start(vscode_lsp, permanent) of
        {ok, Started} -> ok;
        {error, Reason} -> io:format("application start error : ~p",[Reason]), {error, Reason};
        _ -> ok
        end;
    {error, Reason} -> 
        io:format("compile_needed_modules failed : ~p",[Reason]),
        {error, Reason}
    end.

compile_needed_modules() ->
    CompileOptions = [verbose, binary, report],
    do_compile(["src/vscode_lsp_app", "src/gen_lsp_server", 
        "src/gen_lsp_sup", "src/gen_connection", "src/vscode_jsone"], CompileOptions)
    .

do_compile([H|T], CompileOptions) ->
    %compile in memory
    case compile:file(H, CompileOptions) of
    {ok, ModuleName, Binary} -> 
        case code:load_binary(ModuleName, atom_to_list(ModuleName), Binary) of
        {module, _} ->
            do_compile(T, CompileOptions);
        {error, Reason} -> 
            {error, Reason}
        end;
    _Any -> 
        io:format("compile result : ~p", [_Any]),
        {error, _Any}

    end;

do_compile([], CompileOptions) ->
    ok.