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

	% after we have joined, list the clients

	% Room_pid ! {list_clients, self()},

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

	io:format("handle_request ~p~n", [Type]),

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

	io:format("do_request: ~p~n", [Message_out]),

	%% message is probably json & get decoded
	Message_out1 = Message_out,

	io:format("do_request 2: ~p~n", [Message_out1]),

	Message_out2 = Message_out1, 
	io:format("sarg_websocket_cb:doRequest ~n", []),
	{ binary_to_list(Type), Message_out2}.

% Any proccessing which needs to happen to the message
format_message("message", Message, Socket) -> 
	{ok, {Address, Port}} = inet:peername(Socket),

	Message2 = binary_to_list(Message),

	list_to_binary((io_lib:format("~p", [Address])) ++ ":" ++ (io_lib:format("~p", [Port]))  ++ ": " ++ Message2)
	;
format_message(_Type, Message, _Socket) -> 
	% mochijson2:encode( Message )
	Message
.

get_to({struct, [{<<"to">>, To } | _]}) -> 
	binary_to_list(To)
;
get_to({struct, [ _H | T]}) -> 
	get_to({struct, T})
.

% handle_info({client_list, Users}, {Room_pid, _Users}) -> 
	
% 	io:format("sarg_websocket_cb: handle_info: client_list ~n", []),

% 	% filter the client list, to remove ourselfs

% 	% TODO

% 	%put in dict
% 	Dict1 = dict:new(),
% 	Dict2 = toDict(Users, Dict1),

% 	% add to state
% 	State2 = {Room_pid, Dict2},

% 	io:format("cb: handle_info: dict: ~p~n", [Dict2]),
% 	% send list to the client
% 	Keys = dict:fetch_keys(Dict1),

% 	Message_out = mochijson2:encode({struct, [
% 		{type, <<"client_list">>}
% 		,{message, [ "array" | Keys ] }
% 	]}),

% 	% gen_server:cast(Room_pid, {message_out, Message_out}), % send to the client
% 	websocket:talk(self(), Message_out),

% 	{noreply, State2}
% .




