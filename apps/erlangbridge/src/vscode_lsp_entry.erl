-module(vscode_lsp_entry).


-export([start/0, start/1]).


start() ->
    error_logger:info_msg("vscode_lsp_entry started/0"),
    init_lsp().

start(_Args) ->
    %%TODO activate or not report depends on command line 
    %%error_logger:tty(false),    
    error_logger:info_msg("vscode_lsp_entry started/1(~p)", [_Args]),
    init_lsp().


init_lsp() ->
    case  compile_needed_modules() of
    ok ->
        case application:start(vscode_lsp, permanent) of
        {ok, _Started} -> ok;
        {error, Reason} -> 
            error_logger:error_msg("application start error : ~p",[Reason]), 
            {error, Reason};
        _ -> ok
        end;
    {error, Reason} -> 
        error_logger:error_msg("compile_needed_modules failed : ~p",[Reason]),
        {error, Reason}
    end.

compile_needed_modules() ->
    CompileOptions = [verbose, binary, report],
    do_compile(["src/vscode_lsp_app", "src/gen_lsp_server", 
        "src/gen_lsp_sup", "src/gen_lsp_doc_sup","src/gen_lsp_doc_server",
        "src/vscode_lsp_app_sup", "src/lsp_navigation", "src/lsp_syntax",
        "src/gen_connection", "src/vscode_jsone","src/hover_doc_layout"], CompileOptions)
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
        error_logger:error_msg("compile result of ~p: ~p",[H, _Any]),
        {error, _Any}

    end;

do_compile([], _CompileOptions) ->
    ok.