-module(websocket_sup).

-behaviour(supervisor).

-export([start_link/0, get_spec/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [])
	.

init([]) -> 	
	{ok, L} = gen_tcp:listen( 
		8079, 
		[binary, 
			{active, true}, 
			{reuseaddr, true}, 
			{packet, 0}
		]
	),
	Server = get_spec(L), 
	Children = [Server], 
	RestartStrategy = {one_for_one, 0, 1},

	gen_event:start_link({local, websocket_man}),
	gen_event:add_handler(websocket_man, websocket_evt, [self(), L]),
	% tell the websocket_man that we have had a connection added
	% gen_event:notify(websocket_man, websocket_connected),

	{ok, {RestartStrategy, Children}}
	.

get_spec(L) -> 
	{make_ref(), %name
		{sarg_websocket_cb, start_link, [L]}, %start child 
		temporary, %temporary, permanant, transiant.
		2000, % shut down timeout in ms
		worker, % its a worker, it does stuff
		[ websocket, mochijson2] % dependant modules
	}.
