-module(erlbal).
-export([make_request/2, start_bal/1, kill_bal/1, start_server/2]).

start_server(Balancer, Fun) ->
	PID = spawn(fun() -> server_loop(Balancer, Fun) end),
	Balancer ! {add_node, PID}.

server_loop(Balancer, Fun) ->
	receive
		{request, From, ARGS} ->
			Ret = Fun(ARGS),
			Balancer ! {response, From, Ret},
			server_loop(Balancer, Fun);
		die ->
			ok
	end.

start_bal(Name) ->
	register(Name, spawn(fun() -> bal_loop([], 1) end)).

kill_bal(Balancer) ->
	Balancer ! die,
	unregister(Balancer).

bal_loop(Serverlist, Nextserver) ->
	receive
		{add_node, PID} ->
			bal_loop(Serverlist ++ [PID], Nextserver);
		{request, From, ARGS} ->
			Serv = lists:nth(Nextserver, Serverlist),
			Serv ! {request, From, ARGS},
			NS = Nextserver + 1,
			SLL = length(Serverlist),
			if
				NS > SLL ->
					bal_loop(Serverlist, 1);
				true ->
					bal_loop(Serverlist, Nextserver+1)
			end;
		{response, To, Ret} ->
			To ! Ret,
			bal_loop(Serverlist, Nextserver);
		die ->
			lists:foreach(fun(X) -> X ! die end, Serverlist)
	end.


make_request(Balancer, ARGS) ->
	Balancer ! {request, self(), ARGS},
	receive
		ID ->
			ID
	end.
