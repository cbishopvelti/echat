-module(room_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() -> 
	Link = supervisor:start_link({global, ?MODULE}, ?MODULE, []), 
	Link.

init([]) ->
	Home_room = {home_sup, %room name
		{room_srv, start_link, [home]},
		permanent,
		2000, 
		worker, 
		[]
	}, 

	Children = [Home_room],
	RestartStrategy = {one_for_one, 3, 60}, % 3 restart attempts a minute

	{ok, {RestartStrategy, Children}}
	.


