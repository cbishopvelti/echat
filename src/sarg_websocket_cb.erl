-module(sarg_websocket_cb).

-export([start_link/1]).
%callbacks
-export([init/1, handle_request/3]).



start_link(L) -> 
	websocket_srv:start_link(L, ?MODULE, []).

% fires after connection has been initialised
init([]) -> 
	% connect us to a room
	{ok, Room_pid} = gen_server:call(global:whereis_name( home ), {join}),

	%connect us up to a room
	{ok, {Room_pid, null}}.

%handle request from server
handle_request(Message, Socket, State) -> 
	% decode the json
	Message2 = mochijson2:decode(Message),

	{Room_pid, _Users} = State,

	% bad, assumes type will always be first
	{struct, [{<< "type" >>, Type3} | _T]} = Message2,
	Type = binary_to_list(Type3),

	if 
		Type == "message" -> 

			{ _Type, Message_out } = do_request(Message2),

			Message_out1 = format_message(Type, Message_out, Socket),

			Message_out2 = mochijson2:encode({struct, [{type, list_to_binary(Type)}, {message, Message_out1 }]}),

			gen_server:cast(Room_pid, {send, Message_out2})
		;
		Type == "connect_media" -> 
			Room_pid ! { connect_media, self() }
		;
		true -> 
			% just send the raw message though
			To = get_to(Message2), 
			gen_server:cast(Room_pid, {send_to, To, Message})
	end, 

	{ok, State}.

do_request({struct, [{<<"type">>, Type} | T]}) -> 
	[H2 | _] = T,
	{<<"message">>, Message_out} = H2,

	{ binary_to_list(Type), Message_out}.

% Any proccessing which needs to happen to the message
format_message("message", Message, Socket) -> 
	{ok, {Address, Port}} = inet:peername(Socket),

	Message2 = binary_to_list(Message),

	list_to_binary((io_lib:format("~p", [Address])) ++ ":" ++ (io_lib:format("~p", [Port]))  ++ ": " ++ Message2)
	;
format_message(_Type, Message, _Socket) -> 

	Message
.

get_to({struct, [{<<"to">>, To } | _]}) -> 
	binary_to_list(To)
;
get_to({struct, [ _H | T]}) -> 
	get_to({struct, T})
.



