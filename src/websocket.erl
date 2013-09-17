-module(websocket).
-export([ handshake_and_talk/1,
	demask/2,
	talk/2
]).
	
handshake_and_talk(Ppid) -> 
	receive
		{Ppid, X} -> 
			AcceptKey = process_client_handshake(binary_to_list(X)), 
			
			
			Ppid ! {send, self(), "HTTP/1.1 101 Switching Protocols\r\n"}, 
			Ppid ! {send, self(), "Upgrade: websocket\r\n"}, 
			Ppid ! {send, self(), "Connection: Upgrade\r\n"}, 
			Ppid ! {send, self(), string:concat(string:concat("Sec-WebSocket-Accept: ", AcceptKey), "\r\n")},
			Ppid ! {send, self(), "Sec-WebSocket-Protocol: chat\r\n"}, 
			Ppid ! {send, self(), "\r\n"}, 
			
			Ppid ! {self(), connected};

		Any -> io:format("[Child] Random stuff received: ~p~n. ~p~n", [Any, Ppid])
	end.

talk(Pid, Out) -> 
	Out2 = 
		if
			is_bitstring(Out) -> Out;
			true -> list_to_binary(Out)
		end,

	OutSize = websocket_length(Out2), 
	Pid ! {send, self(), << 16#81, OutSize/binary, Out2/binary >>},
	{ok}.
	
websocket_length(Bin) -> 
	if 
		(byte_size(Bin) =< 125)
			-> 
				OutBin = binary:encode_unsigned(byte_size(Bin)),
				<< OutBin/binary >>;
		( (byte_size(Bin) >= 126) and (byte_size(Bin) =< 65535)  )
			->
				OutBin = binary:encode_unsigned(byte_size(Bin)),
				<< 16#7E, 0:((2 - (size(OutBin) rem 8)) * 8),OutBin/binary>> ;
		( (byte_size(Bin) >= 65536) and (byte_size(Bin) =< 16#7FFFFFFFFFFFFFFF))
			->
				OutBin = binary:encode_unsigned(byte_size(Bin)),
				<< 16#7F, 0: ((8 - (size(OutBin) rem 8 )) * 8), OutBin/binary>>;
		true
			-> 
				% TODO: implement fragmented messages, but will never happen
				% unless someone wants to send a message > 16#7FFFFFFFFFFFFFFF
				error
	end.
		
	
	
% Process client's handshake to retrieve information. 
process_client_handshake(X)-> 
    [_Body|Head] = lists:reverse(string:tokens(X, "\r\n")), 
    {Key1} = extract_keys(lists:reverse(Head)), 
	Key2 = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
	Key3 = string:concat(Key1, Key2),
	Key4 = crypto:sha(list_to_binary(Key3)),
	binary_to_list(base64:encode(Key4)).
	


% Extract keys from the client's handshake. 
extract_keys([H|T])-> 
    Key = extract_key("Sec-WebSocket-Key: ", [H|T]), 
    {Key}. 

extract_key(X, [H|T])-> 
    case string:str(H, X) of 
        0 -> extract_key(X, T); 
        _Pos -> string:substr(H, string:len(X) + 1) 
    end. 
			
% demask/2 Data, is the size + data, with or without the masking bit
demask(<<H:8/integer, T/binary>>, SState) when (16#7f band H) =< 125 -> 
	S = (16#7f band H), %remove the masking bit
	demask(S, T, SState);
demask(<<H:8/integer, S:16/integer-unsigned, T/binary>>, SState) when (16#7f band H) == 126 ->
	demask(S, T, SState);
demask(<<H:8/integer, S:64/integer-unsigned, T/binary>>, SState) when (16#7f band H) == 127 ->
	demask(S, T, SState);
demask(<< _T/binary >>, SState) -> % if the above doesn't match, then our header has probably been split into multiple tcp packages and we need to buffer
	{buffer, SState}
.


%% demask/3 after we know the size
demask(Size, <<Mask:4/binary, Data/binary>>, SState) when (byte_size(Data) == Size) -> 

	{demask2(Mask, Data), SState} %% actualy do the demasking
	;
demask(Size, <<Mask:4/binary, Data/binary>>, SState) when (byte_size(Data) > Size) -> % chrome can send 2 websocket packets together

	<<ToDecode:Size/binary, Rest/binary>> = <<Data/binary>>,

	{noreply, SState2} = websocket_srv:handle_info({tcp, null, <<Rest/binary>>}, SState),	

	% demask2(Mask, ToDecode)
	{demask2(Mask, ToDecode), SState2}
	;
demask(Size, <<_Mask:4/binary, Data/binary>>, SState) when (byte_size(Data) < Size) -> % the data is accross multiple packets
	{ buffer, SState }
	;
demask(_Size, <<_Stuff/binary>>, SState) -> % haven't even recieved the full header
	{ buffer, SState }
	.

%% actualy does the demasking
demask2(<<Mask:4/binary>>, <<H:4/binary, T/binary>>) -> 
	
	<< Mask2:32/integer >> = Mask, 

	<< H2:32/integer >> = H,

	Out = Mask2 bxor H2, 

	Out2 = binary_to_list(<< Out:32/integer >>),

	Out2 ++ demask2(Mask, T);

demask2(<<Mask:4/binary>>, <<End/binary>>) when (size(End) > 0 ) -> 
	
	EndSize = byte_size(End), 
	<< PMask:EndSize/binary, _Tail/binary>> = Mask,
	EndBitSize = (8 * EndSize),
	<< PMask2:EndBitSize/integer >> = PMask,

	<< End2:EndBitSize/integer >> = End,

	Out = PMask2 bxor End2,
	Out2 = binary_to_list(<< Out:EndBitSize/integer >>),

	Out2; % back to binary
%% endo conditions
demask2(<< _Mask:4/binary >>, << >>) -> 
	"".










