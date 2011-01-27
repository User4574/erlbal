-module(erlbal).
-export([make_request/0, start_bal/0, kill_bal/0, start_server/1]).

start_server(ID) ->
	PID = spawn(fun() -> serverloop(ID) end),
	balancer ! {add_node, PID}.

serverloop(ID) ->
	receive
		{request, From} ->
			balancer ! {response, From, ID},
			serverloop(ID);
		die ->
			ok
	end.

start_bal() ->
	register(balancer, spawn(fun() -> balloop([], 1) end)).

kill_bal() ->
	balancer ! die.

balloop(Serverlist, Nextserver) ->
	receive
		{add_node, PID} ->
			balloop(Serverlist ++ [PID], Nextserver);
		{request, From} ->
			Serv = lists:nth(Nextserver, Serverlist),
			Serv ! {request, From},
			NS = Nextserver + 1,
			SLL = length(Serverlist),
			if
				NS > SLL ->
					balloop(Serverlist, 1);
				true ->
					balloop(Serverlist, Nextserver+1)
			end;
		{response, To, ID} ->
			To ! ID,
			balloop(Serverlist, Nextserver);
		die ->
			lists:foreach(fun(X) -> X ! die end, Serverlist)
	end.


make_request() ->
	balancer ! {request, self()},
	receive
		ID ->
			ID
	end.
