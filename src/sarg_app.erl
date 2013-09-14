-module(sarg_app).

-behaviour(application).

-export([
	start/2,
	stop/1
]).

start(_Type, _StartArgs) ->
	io:format("sarg_app: start ~p~n", [self()]),
	sarg_sup:start_link()
	.
	
stop(_State) ->
	ok.
