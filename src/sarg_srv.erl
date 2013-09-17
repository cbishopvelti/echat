-module(sarg_srv).

-behavour(gen_server).

-export([
	start_link/0,
	stop/0, 
	service/3
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])
.

stop() -> 
	ok
.
	
init([]) -> 
	{ok, {}, 0}.

handle_call(_Request, _From, State) -> 
	{reply, ok, State}.
handle_cast(_Message, State) -> 
	{noreply, State}.

handle_info(timeout, _State) -> 
	inets:start(httpd, [
		{port, 8080},
		{server_name, "localhost"},
		{modules, [
			mod_alias,
			mod_esi,
			mod_get, %%for serving static files.
			mod_log,
			mod_disk_log
		]},
		{erl_script_alias, {"/kill", [sarg_srv]}}, 
		{server_root, "server"}, 
		{document_root, "server/htdocs"}, 
		{error_log, "error.log"},
		{security_log, "security.log"},
		{transfer_log, "transfer.log"},
		{bind_address, 
			{0,0,0,0}
		},
		{mime_types, [
			{"html", "text/html"}
		]}
	]), 
	{noreply, {}}
	.
% if we terminate, stop inets.
terminate(_Reason, {Pid}) -> 
	inets:stop(httpd, Pid),
	ok.

service(SessionID, _Env, _Input) -> 

	mod_esi:deliver(SessionID, [
		"Host: localhost\r\n\r\n
		Upgrade: WebSocket\r\n\r\n
		Content-Type: text/html\r\n\r\n",
		"<html><body>Hello World</body></html>"
	]), 
	receive
		_From -> {ok}
	end.

code_change(_OldVsn, State, _Extra) -> 
	{noreply, State}.
