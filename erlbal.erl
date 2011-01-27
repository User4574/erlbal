-module(erlbal).
% test
-export([make_request/2, start_bal/0, stop_bal/1, start_server/2, stop_server/2, list_servers/1]).

start_server(Balancer, Fun) ->
	PID = spawn(fun() -> server_loop(Balancer, Fun) end),
	Balancer ! {add_node, PID}.

stop_server(Balancer, PID) ->
	Balancer ! {del_node, PID},
	PID ! die.

list_servers(Balancer) ->
	Balancer ! {list_nodes, self()},
	receive Nodes -> Nodes end.

server_loop(Balancer, Fun) ->
	receive
		{request, From, ARGS} ->
			Ret = Fun(ARGS),
			Balancer ! {response, From, Ret},
			server_loop(Balancer, Fun);
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
		{response, To, Ret} ->
			To ! Ret,
			bal_loop(Serverlist, Nextserver);
		die ->
			lists:foreach(fun(X) -> X ! die end, Serverlist)
	end.


make_request(Balancer, ARGS) ->
	Balancer ! {request, self(), ARGS},
	receive Ret -> Ret end.
