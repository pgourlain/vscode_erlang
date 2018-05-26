-module(gen_lsp_server).

-behavior(gen_server).

%inspired from https://github.com/kevinlynx/erlang-tcpserver/blob/master/test/test.erl
%http://20bits.com/article/erlang-a-generalized-tcp-server

% à regarder
% http://learnyousomeerlang.com/buckets-of-sockets


%API
-export([start_link/1, start_link/2]).

%export for gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([send_to_client/2]).

-define(SERVER, ?MODULE).
%state
-record(state, {socket, content_length, contents}).

start_link(VsCodePort) ->
    start_link(VsCodePort, undefined).

start_link(VsCodePort, Socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [VsCodePort, Socket, self()],[]).

init([_VsCodePort, Socket, _Parent]) ->
    {ok, #state{socket = Socket, contents = <<"">>}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, Contents}, State) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, handle_tcp_data(Socket, Contents, State)};
handle_info(timeout, #state{socket = Socket} = State) ->
    {ok, _} = gen_tcp:accept(Socket), 
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(Data, State) ->
    {noreply, State}.

remove_text_for_logging(#{params := #{contentChanges := ChangesList} = Params} = Input) ->
    Input#{params := Params#{contentChanges := lists:map(fun 
        (#{text := <<Text/binary>>} = Change) when byte_size(Text) > 20 ->
            Cut = binary:part(Text, 0, 20),
            Change#{text := <<Cut/binary, " ...">>};
        (Change) ->
            Change
    end, ChangesList)}};
remove_text_for_logging(#{params := #{textDocument := #{text := Text} = TextDocument} = Params} = Input) when byte_size(Text) > 20 ->
    Cut = binary:part(Text, 0, 20),
    Input#{params := Params#{textDocument := TextDocument#{text := <<Cut/binary, " ...">>}}};
remove_text_for_logging(Input) ->
    Input.

do_contents(Socket, #{method := Method} = Input) ->
    error_logger:info_msg("LSP received ~p", [remove_text_for_logging(Input)]),
    Handler = list_to_atom(binary_to_list(binary:replace(Method, <<"/">>, <<"_">>))),
    case lists:keyfind(Handler, 1, lsp_handlers:module_info(exports)) of
        false ->
            error_logger:error_msg("Method not handled: ~p", [Method]),
            ok;
        {Function, 2} ->
            Result = apply(lsp_handlers, Function, [Socket, maps:get(params, Input, undefined)]),
            case maps:get(id, Input, undefined) of
                undefined ->
                    ok;
                Id ->
                    send_to_client(Socket, #{id => Id, result => Result})
            end
    end;
do_contents(Socket, #{id := Id} = Input) ->
    error_logger:info_msg("LSP received ~p", [Input]),
    Handler = list_to_atom(binary_to_list(binary:replace(Id, <<"/">>, <<"_">>))),
    case lists:keyfind(Handler, 1, lsp_handlers:module_info(exports)) of
        false ->
            error_logger:error_msg("Notification not handled: ~p ~p", [Id, Input]),
            ok;
        {Function, 2} ->
            apply(lsp_handlers, Function, [Socket, maps:get(result, Input, undefined)])
    end.

send_to_client(Socket, Body) ->
    error_logger:info_msg("LSP sends ~p", [Body]),
    {ok, Json} = vscode_jsone:encode(Body),
    Header = iolist_to_binary(io_lib:fwrite("Content-Length: ~p", [byte_size(Json)])),
    gen_tcp:send(Socket, <<Header/binary, "\r\n\r\n", Json/binary>>).

handle_tcp_data(Socket, Contents, State) ->
    StateWithContents = State#state{contents = <<(State#state.contents)/binary, Contents/binary>>},
    StateWithLength = case StateWithContents#state.content_length of
        undefined ->
            HeadersEnd = binary:match(StateWithContents#state.contents, <<"\r\n\r\n">>),
            case HeadersEnd of
                nomatch ->
                    StateWithContents;
                {HeadersSeparatorStart, HeadersSeparatorLen} ->
                    {match, [_, {LengthStart, LengthLen}]} =
                        re:run(StateWithContents#state.contents, "Content-Length: *([0-9]+)"),
                    Length = binary_to_integer(binary:part(StateWithContents#state.contents, LengthStart, LengthLen)),
                    BodyStart = HeadersSeparatorStart + HeadersSeparatorLen,
                    BodyLen = byte_size(StateWithContents#state.contents) - BodyStart,
                    StateWithContents#state{
                        contents = binary:part(StateWithContents#state.contents, BodyStart, BodyLen),
                        content_length = Length
                    }
            end;
        _ ->
            StateWithContents
    end,
    case StateWithLength#state.content_length of
        undefined ->
            StateWithLength;
        ContentLength when ContentLength > byte_size(StateWithLength#state.contents) ->
            StateWithLength;
        ContentLength when ContentLength =:= byte_size(StateWithLength#state.contents) ->
            {ok, Input, _} = vscode_jsone_decode:decode(StateWithLength#state.contents, [{keys, atom}]),
            do_contents(Socket, Input),
            StateWithLength#state{contents = <<"">>, content_length = undefined};
        ContentLength when ContentLength < byte_size(StateWithLength#state.contents) ->
            ShorterContents = binary:part(StateWithLength#state.contents, 0, ContentLength),
            {ok, Input, _} = vscode_jsone_decode:decode(ShorterContents, [{keys, atom}]),
            do_contents(Socket, Input),
            handle_tcp_data(
                Socket,
                binary:part(StateWithLength#state.contents, ContentLength, byte_size(StateWithLength#state.contents) - ContentLength),
                StateWithLength#state{contents = <<"">>, content_length = undefined})
    end.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
