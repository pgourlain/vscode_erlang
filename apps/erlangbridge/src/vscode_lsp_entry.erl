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
            vscode_lsp_log:error(<<"lsp/app">>,"application start error : ~p",[Reason]), 
            {error, Reason};
        _ -> ok
        end;
    {error, Reason} -> 
        vscode_lsp_log:error(<<"lsp/app">>,"compile_needed_modules failed : ~p",[Reason]),
        {error, Reason}
    end.

compile_needed_modules() ->
    %% No `verbose`: it makes the compiler print "Old inliner: ..." for the
    %% -compile({inline, ...}) attributes in vscode_jsone* on every start.
    CompileOptions = [binary, report],
    %% vscode_lsp_log first - do_compile/2 itself logs through it.
    do_compile(["src/vscode_lsp_log", "src/vscode_lsp_app", "src/vscode_gen_lsp_server", 
        "src/vscode_gen_lsp_sup", "src/vscode_gen_lsp_doc_sup","src/vscode_gen_lsp_doc_server", "src/vscode_gen_lsp_config_sup","src/vscode_gen_lsp_config_server",
        "src/vscode_gen_lsp_help_sup","src/vscode_gen_lsp_help_server", "src/vscode_lsp_handlers", "src/vscode_lsp_utils",
        "src/vscode_lsp_app_sup", "src/vscode_lsp_navigation", "src/vscode_lsp_signature", "src/vscode_lsp_parse", "src/vscode_lsp_syntax", "src/vscode_lsp_completion", "src/vscode_lsp_inlayhints",
        "src/vscode_gen_connection", "src/vscode_jsone","src/vscode_jsone_decode","src/vscode_hover_doc_layout", "src/vscode_lsp_worker", "src/vscode_lsp_rename",
        "src/vscode_lsp_fun_utils", "src/vscode_lsp_signature_doc_layout", "src/vscode_lsp_codeaction", "src/vscode_lsp_semantic_tokens",
        "src/vscode_lsp_workspace_symbol", "src/vscode_lsp_hierarchy", "src/vscode_lsp_folding", "src/vscode_lsp_formatting",
        "src/vscode_lsp_testing", "src/vscode_lsp_testing_eunit_report", "src/vscode_lsp_testing_ct_hook",
        "src/vscode_lsp_diagnostics"], CompileOptions)
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
        vscode_lsp_log:error(<<"lsp/app">>,"compile result of ~p: ~p",[H, _Any]),
        {error, _Any}

    end;

do_compile([], _CompileOptions) ->
    ok.
