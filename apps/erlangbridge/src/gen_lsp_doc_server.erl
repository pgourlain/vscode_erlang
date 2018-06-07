-module(gen_lsp_doc_server).

-behavior(gen_server).
-export([start_link/0]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([set_document_attribute/3, remove_document/1, get_document_attribute/2, get_documents/0]).
-export([root_available/0, project_modules/0, add_project_file/1, remove_project_file/1, get_module_file/1]).

-define(SERVER, ?MODULE).

-record(state, {opened, project_modules}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[]).

set_document_attribute(File, Attribute, Value) -> 
    gen_server:call(?SERVER, {set_document_attribute, File, Attribute, Value}).

remove_document(File) ->
    gen_server:cast(?SERVER, {remove, File}).

get_document_attribute(File, Attribute) ->
    gen_server:call(?SERVER, {get_document_attribute, File, Attribute}).

get_documents() ->
    gen_server:call(?SERVER, get_documents).

root_available() ->
    gen_server:cast(?SERVER, root_available).

project_modules() ->
    gen_server:call(?SERVER, project_modules).

add_project_file(File) ->
    gen_server:cast(?SERVER, {add_project_file, File}).

remove_project_file(File) ->
    gen_server:cast(?SERVER, {remove_project_file, File}).

get_module_file(Module) ->
    gen_server:call(?SERVER, {get_module_file, Module}).

init(_Args) ->
    {ok, #state{opened = #{}, project_modules = #{}}}.

handle_call({get_document_attribute, File, Attribute}, _From, State) ->
    {reply, proplists:get_value(Attribute, maps:get(File, State#state.opened, [])), State};

handle_call({set_document_attribute, File, Attribute, Value},_From, State) ->
    Opened = State#state.opened,
    Attributes = maps:get(File, Opened, []),
    UpdatedAttributes = case proplists:is_defined(Attribute, Attributes) of
        true ->
            lists:keyreplace(Attribute, 1, Attributes, {Attribute, Value});
        _ ->
            [{Attribute, Value} | Attributes]
    end,
    {reply, ok, State#state{opened = Opened#{File => UpdatedAttributes}}};

handle_call(get_documents, _From, State) ->
    {reply, maps:keys(State#state.opened), State};

handle_call(project_modules, _From, State) ->
    {reply, maps:keys(State#state.project_modules), State};

handle_call({get_module_file, Module},_From, State) ->
    File = case maps:get(atom_to_list(Module), State#state.project_modules, []) of
        [] ->
            undefined;
        [OneFile] ->
            OneFile;
        [AFile|Rest] ->
            BuildElements = filename:split(gen_lsp_config_server:root()) ++ ["_build"],
            NoBuildFiles = lists:filter(fun (Filename) ->
                not lists:prefix(BuildElements, filename:split(Filename))
            end, [AFile|Rest]),
            case NoBuildFiles of
                [ANoBuioldFile|_] ->
                    ANoBuioldFile;
                _ ->
                    AFile
            end
    end,
    {reply, File, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({remove, File}, State) ->
    Opened = State#state.opened,
    {noreply, State#state{opened = maps:remove(File, Opened)}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(root_available, State) ->
    ProjectModules = filelib:fold_files(gen_lsp_config_server:root(), ".erl$", true, fun (Found, Acc) ->
        Module = filename:rootname(filename:basename(Found)),
        UpdatedFiles = [Found | maps:get(Module, Acc, [])],
        Acc#{Module => UpdatedFiles}
    end, #{}),
    {noreply, State#state{project_modules = ProjectModules}};

handle_cast({add_project_file, File}, State) ->
    Module = filename:rootname(filename:basename(File)),
    UpdatedFiles = [File | maps:get(Module, State#state.project_modules, [])],
    UpdatedProjectModules = (State#state.project_modules)#{Module => UpdatedFiles},
    {noreply, State#state{project_modules = UpdatedProjectModules}};

handle_cast({remove_project_file, File}, State) ->
    Module = filename:rootname(filename:basename(File)),
    UpdatedFiles = lists:delete(File, maps:get(Module, State#state.project_modules, [])),
    UpdatedProjectModules = case UpdatedFiles of
        [] -> maps:remove(Module, State#state.project_modules);
        _ -> (State#state.project_modules)#{Module => UpdatedFiles}
    end,
    {noreply, State#state{project_modules = UpdatedProjectModules}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
