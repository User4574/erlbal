-module(erlbal_n).
-export([make_request/2, start_bal/0, stop_bal/1, start_server/5, stop_server/2, list_servers/1]).

start_server(Balancer, Node, Fun, LoadFun, INITSTATE) when is_function(Fun, 2) andalso is_function(LoadFun, 0) andalso is_list(INITSTATE) ->
	PID = spawn(Node, fun() -> server_loop(Fun, LoadFun, INITSTATE) end),
	Balancer ! {add_node, PID}.

stop_server(Balancer, PID) ->
	Balancer ! {del_node, PID},
	PID ! die.

list_servers(Balancer) ->
	Balancer ! {list_nodes, self()},
	receive Nodes -> Nodes end.

server_loop(Fun, LoadFun, STATE) ->
	receive
		{request, From, Serverlist, ARGS} ->
			{Ret, NEWSTATE} = Fun(ARGS, STATE),
			From ! Ret,
			updatestates(Serverlist, self(), NEWSTATE),
			server_loop(Fun, LoadFun, NEWSTATE);
		{load, From} ->
			From ! LoadFun(),
			server_loop(Fun, LoadFun, STATE);
		{update, NEWSTATE} ->
			server_loop(Fun, LoadFun, NEWSTATE);
		die ->
			ok
	end.

updatestates([], _, _) ->
	ok;
updatestates([H|T], H, NEWSTATE) ->
	updatestates(T, H, NEWSTATE);
updatestates([H|T], P, NEWSTATE) ->
	H ! {update, NEWSTATE},
	updatestates(T, P, NEWSTATE).

start_bal() ->
	spawn(fun() -> bal_loop([]) end).

stop_bal(Balancer) ->
	Balancer ! die.

bal_loop(Serverlist) ->
	receive
		{add_node, PID} ->
			bal_loop(Serverlist ++ [PID]);
		{del_node, PID} ->
			bal_loop(Serverlist -- [PID]);
		{list_nodes, From} ->
			From ! Serverlist,
			bal_loop(Serverlist);
		{request, From, ARGS} ->
			if
				length(Serverlist) > 0 ->
					PID = bal_worker(Serverlist),
					PID ! {request, From, Serverlist, ARGS},
					bal_loop(Serverlist);
				true ->
					bal_loop(Serverlist)
			end;
		die ->
			ok
	end.

bal_worker([H|T]) ->
	H ! {load, self()},
	receive
		N ->
			bal_worker(T, N, H)
	end.
bal_worker([], _, PID) ->
	PID;
bal_worker([H|T], Best, PID) ->
	H ! {load, self()},
	receive
		N ->
			if 
				N > Best ->
					bal_worker(T, N, H);
				true ->
					bal_worker(T, Best, PID)
			end
	end.
	

make_request(Balancer, ARGS) when is_list(ARGS)->
	Balancer ! {request, self(), ARGS},
	receive Ret -> Ret end.
