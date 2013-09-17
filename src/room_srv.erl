-module(room_srv).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Name) -> 
	gen_server:start_link({global, Name}, ?MODULE, [Name], [])
	.

init([Name]) -> 
	{ok, {Name, []}, 0}.

handle_call({join}, From, {Name, Users}) ->

	{Pid, _Ref} = From,

	% insert the user
	% only join when media is available
	ets:insert(Users, {erlang:ref_to_list(make_ref()), Pid, 0}),
	erlang:monitor(process, Pid),

	{reply, {ok, self()}, {Name, Users}}
	;
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) -> 
	{stop, normal, State};

% send to everyone in the room
handle_cast({send, Message}, State) -> 
	{_Name, Users} = State,
	send_messages(Message, Users),
	{noreply, State};

% send to one person in the room
handle_cast({send_to, To, Message}, State) -> 
	{_Name, Users} = State,
	try
		[{_, To_pid, _} | _T] = ets:lookup(Users, To), 
		gen_server:cast(To_pid, {message_out, Message}),
		{noreply, State}
	catch
		% don't do anything if user doesn't exist
		error:{badmatch, _Reason} -> {noreply, State}
	end
;


handle_cast(_Request, State) -> 
	{noreply, State}.


handle_info({connect_media, Reply_pid}, State) -> 
	{_Name, Users} = State,

	% to
	[ To | _T] = ets:match(Users, {'$1', Reply_pid, '_'}),

	[To2 | _T2] = To,

	%update so we can send media
	ets:update_element(Users, To2, {3, 1}),
		
	% get all clients except this one

	Reply = ets:select(Users, [{
		{'$1', '$2', '$3'}, 
		[{'/=', '$2', Reply_pid}, {'==', '$3', 1}],
		['$1']
	}]),

	Reply2 = list_of_list_to_list_of_binary(Reply),

	%send it to the websocket
	Out = mochijson2:encode({struct, [
		{ type, <<"client_list">> },
		{ to, list_to_binary(To) },
		{ message, Reply2 }
	]}), 

	gen_server:cast(Reply_pid, { message_out, Out }), 

	{noreply, State}
;
handle_info(timeout, {Name, _Users}) -> 
	Users = ets:new(users, [set]), 

	{noreply, {Name, Users}}
;
handle_info({'DOWN', _Ref, process, Pid, _Reason}, {Name, Users}) -> 
	%delete droped pids from table.
	ets:match_delete(Users, {'_', Pid, '_'}),
	{noreply, {Name, Users}}
;
handle_info(_Request, State) -> 
	{noreply, State}
.


code_change(_OldVsn, State, _Extra) -> 
	{noreply, State}.

terminate(_Reason, _State) -> 
	ok.

send_messages(Message, Users) -> 
	send_messages(Message, Users, ets:first(Users))
.
send_messages(_Message, _Users, '$end_of_table') -> 
	ok;
send_messages(Message, Users, Ref1) -> 
	[{_Ref, User_pid, _} | _T] = ets:lookup(Users, Ref1), % _T should be empty
	gen_server:cast(User_pid, {message_out, Message}),
	send_messages(Message, Users, ets:next(Users, Ref1))
.


list_of_list_to_list_of_binary([]) -> 
[];
list_of_list_to_list_of_binary([H | T]) -> 
	[ list_to_binary(H) | list_of_list_to_list_of_binary(T)]
.



