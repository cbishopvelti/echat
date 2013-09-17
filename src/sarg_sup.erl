-module(sarg_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() -> 
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) -> 
	Sarg_srv = {sarg_srv, 
		{sarg_srv, start_link, []},
		permanent, 
		2000,
		worker,
		[]
	},

	Websocket_sup = {websocket_sup, 
		{websocket_sup, start_link, []}, 
		permanent, 
		2000, 
		supervisor,
		[websocket, websocket_evt, websocket_srv, mochijson2]
	},

	Room_sup = {room_sup, 
		{room_sup, start_link, []}, 
		permanent, 
		2000, 
		supervisor, 
		[room_srv]
	},

	Children = [
		Sarg_srv,
		Websocket_sup
		,Room_sup
	],
	RestartStratagy = {one_for_one, 3, 60}, % 3 restart attempts a minute

	{ok, {RestartStratagy, Children}}.