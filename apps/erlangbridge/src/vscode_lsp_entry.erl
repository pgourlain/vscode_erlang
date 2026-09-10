-module(vscode_lsp_entry).


-export([start/0, start/1]).


start() ->
    init_lsp().

start(_Args) ->
    init_lsp().

init_lsp() ->
    case  compile_needed_modules() of
    ok ->
        case application:start(vscode_lsp, permanent) of
        {ok, _Started} -> ok;
        {error, Reason} -> 
            lsp_log:error(<<"lsp/app">>,"application start error : ~p",[Reason]), 
            {error, Reason};
        _ -> ok
        end;
    {error, Reason} -> 
        lsp_log:error(<<"lsp/app">>,"compile_needed_modules failed : ~p",[Reason]),
        {error, Reason}
    end.

compile_needed_modules() ->
    CompileOptions = [verbose, binary, report],
    do_compile(["src/vscode_lsp_app", "src/gen_lsp_server", 
        "src/gen_lsp_sup", "src/gen_lsp_doc_sup","src/gen_lsp_doc_server", "src/gen_lsp_config_sup","src/gen_lsp_config_server",
        "src/gen_lsp_help_sup","src/gen_lsp_help_server", "src/lsp_handlers", "src/lsp_utils",
        "src/vscode_lsp_app_sup", "src/lsp_navigation", "src/lsp_signature", "src/lsp_parse", "src/lsp_syntax", "src/lsp_completion", "src/lsp_inlayhints",
        "src/gen_connection", "src/vscode_jsone","src/vscode_jsone_decode","src/hover_doc_layout", "src/worker", "src/lsp_rename",
        "src/lsp_fun_utils", "src/lsp_signature_doc_layout", "src/lsp_codeaction", "src/lsp_semantic_tokens",
        "src/lsp_workspace_symbol", "src/lsp_hierarchy", "src/lsp_folding", "src/lsp_formatting",
        "src/lsp_testing", "src/lsp_testing_eunit_report", "src/lsp_testing_ct_hook", "src/lsp_log"], CompileOptions)
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
        lsp_log:error(<<"lsp/app">>,"compile result of ~p: ~p",[H, _Any]),
        {error, _Any}

    end;

do_compile([], _CompileOptions) ->
    ok.
