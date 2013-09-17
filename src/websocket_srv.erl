-module(websocket_srv).

-behaviour(gen_server).

-export([start_link/3, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% otp 
start_link(L, Callback, Cb_state) -> 
	gen_server:start_link(?MODULE, [L, {Callback, Cb_state}], []).

stop() -> 
	gen_server:cast(?SERVER, stop).

%gen server call backs
init([L, Callback]) -> 
	{ok, {L, Callback, << >>}, 0}
	.

handle_call(_Request, _From, State) -> 
	{reply, ok, State}.

handle_cast(stop, State)->
	{stop, normal, State};

% send message to the client
handle_cast({message_out, Message}, State) -> 
	websocket:talk(self(), Message), %send to client
	{noreply, State};

handle_cast(_Message, State) -> 
	{noreply, State}.


terminate(_Reason, _State) -> 
	ok.



% stop code opp
handle_info({tcp, _Socket, <<16#88, _T/binary >>}, State) ->

	gen_server:cast(self(), stop), %stop as the socket has been closed
	{noreply, State}
;
handle_info({tcp_closed, _Port}, State) -> 
	gen_server:cast(self(), stop),
	{noreply, State}
;

% standard message from the client
handle_info({tcp, Socket, <<16#81:8/integer, Size:8/integer, T/binary>>} , {S, Callback, << _Buff/binary >>}) when (Size band 16#80 == 16#80) -> 

	SizeOut = (Size band 16#7f),

	{ Message, State2 } = websocket:demask(<< SizeOut, T/binary >>, {S, Callback, << >>}),

	{_S2, {Cb2, CbS2}, B2} = State2,

	if 
		Message == buffer -> 
			{noreply, {S, Callback, << 16#81:8/integer, Size:8/integer, T/binary >>}};  % Buff should be empty
		true ->
			{Callback_cb, Callback_state} = {Cb2, CbS2},
			{_R, Callback_state2 } = Cb2:handle_request(Message, Socket, Callback_state),

			{noreply, {S, {Callback_cb, Callback_state2}, << B2/binary >>}}
	end;

handle_info({tcp, Socket, <<Data/binary>>}, {S, Callback, << Buff/binary >>}) when (byte_size(Buff) > 0) ->

	<< Data2/binary >> = << Buff/binary, Data/binary >>, 
	<<16#81:8/integer, Size:8/integer, T/binary>> = << Data2/binary >>,

	SizeOut = (Size band 16#7f), 
	{Message, State2} = websocket:demask(<<SizeOut, T/binary>>, {S, Callback, << >>}), 
	{_S2, {Cb2, CbS2}, B2} = State2,


	if 
		Message == buffer -> 
			{noreply, {S, Callback, << 16#81:8/integer, Size:8/integer, T/binary >>}};
		true ->

			{Callback_cb, Callback_state} = {Cb2, CbS2},
			{_R, Callback_state2 } = Callback_cb:handle_request( Message, Socket, Callback_state ),

			{noreply, {S, {Callback_cb, Callback_state2}, << B2/binary >>}}
	end
;
% socket initialisation request from the client
handle_info({tcp, _Socket, RawData}, {S, Callback, Buff, {handshake, Child}}) ->

	if
		_Socket == null ->
			ok;
		true -> 
			ok
	end,
	Child ! {self(), RawData},
	{noreply, {S, Callback, Buff}};


%call back from handshake that we've connected, so init the websocket callback
handle_info({_Child, connected}, {S, Callback, Buff}) -> 
	{Cb, Cb_state} = Callback,
	{_, Cb_state2} = Cb:init(Cb_state),
	{noreply, {S, {Cb, Cb_state2}, Buff}};

% send data to the client
handle_info({ send, _Child, X}, {S, Callback, Buff}) -> 
	gen_tcp:send(S, X),
	{noreply, {S, Callback, Buff}};
%straight after init, to listen on the socket
handle_info(timeout, {L, Callback, Buff}) -> 
	{ok, S} = gen_tcp:accept(L),

	% connection has been opened, start another.
	gen_event:notify(websocket_man, websocket_connected),
	Pid = self(), 
	HsPid = spawn(fun() -> websocket:handshake_and_talk(Pid) end),
	{noreply, {S, Callback, Buff, {handshake, HsPid}}}
.

code_change(_OldVsn, State, _Extra) -> 
	{noreply, State}.
