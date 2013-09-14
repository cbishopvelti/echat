-module(websocket_evt).

-behaviour(gen_event).

-export([start_link/0]).
-export([init/1, handle_event/2, terminate/2, code_change/3, handle_info/2, handle_call/2]).

start_link() -> 
	gen_event:start_link({locale, tcp_man})
	.

init([Sup_pid, L]) ->
	{ok, {sub_pid, Sup_pid, l, L}}.

% get a new connection ready

handle_event(websocket_connected, State={sub_pid, Sup_pid, l, L}) ->
	% we've been connected, so create another connection waiting to be accepted
	io:format("websocket_evt: handle_event 1 Pid: ~p~n", [Sup_pid]),
	supervisor:start_child(Sup_pid, websocket_sup:get_spec(L)),
	io:format("websocket_evt 002~n", []),
	{ok, State}
	;
handle_event(_Message, State) ->
	io:format("websocket_evt: handle_event 2 ~n", []),
	{ok, State}.

handle_info({'EXIT', _Pid, _Reason}, State) -> 
	{ok, State}.


handle_call(_Request, State) -> 
	io:format("websocket_evt: handle_call ~n", []),
	{ok, {}, State}.

terminate(_Args, _State) -> 
	ok.

code_change(_OldVsn, State, _Extra) -> 
	{noreply, State}.