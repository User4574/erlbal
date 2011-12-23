-module(erlbal_p).
-export([make_request/2, start_bal/0, stop_bal/1, start_server/4, stop_server/2, list_servers/1]).

start_server(Balancer, Node, Fun, INITSTATE) when is_function(Fun, 2) andalso is_list(INITSTATE) ->
	PID = spawn(Node, fun() -> server_loop(Fun, INITSTATE) end),
	Balancer ! {add_node, PID}.

stop_server(Balancer, PID) ->
	Balancer ! {del_node, PID},
	PID ! die.

list_servers(Balancer) ->
	Balancer ! {list_nodes, self()},
	receive Nodes -> Nodes end.

server_loop(Fun, STATE) ->
	receive
		{request, From, ARGS} ->
			{Ret, NEWSTATE} = Fun(ARGS, STATE),
			From ! Ret,
			server_loop(Fun, NEWSTATE);
		die ->
			ok
	end.

start_bal() ->
	spawn(fun() -> bal_loop([], 1) end).

stop_bal(Balancer) ->
	Balancer ! die.

bal_loop(Serverlist, Nextserver) ->
	receive
		{add_node, PID} ->
			bal_loop(Serverlist ++ [PID], Nextserver);
		{del_node, PID} ->
			bal_loop(Serverlist -- [PID], Nextserver);
		{list_nodes, From} ->
			From ! Serverlist,
			bal_loop(Serverlist, Nextserver);
		{request, From, ARGS} ->
			Serv = lists:nth(Nextserver, Serverlist),
			Serv ! {request, From, ARGS},
			NS = Nextserver + 1,
			SLL = length(Serverlist),
			if
				NS > SLL ->
					bal_loop(Serverlist, 1);
				true ->
					bal_loop(Serverlist, NS)
			end;
		die ->
			ok
	end.


make_request(Balancer, ARGS) when is_list(ARGS)->
	Balancer ! {request, self(), ARGS},
	receive Ret -> Ret end.
