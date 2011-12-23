-module(erlbal_p_tests).
-export([start/1]).

start(simple) ->
	Bal = erlbal_p:start_bal(),
	erlbal_p:start_server(Bal, node(), fun(_, _) -> {7, []} end, []),
	erlbal_p:start_server(Bal, node(), fun(_, _) -> {8, []} end, []),
	7 = erlbal_p:make_request(Bal, []),
	8 = erlbal_p:make_request(Bal, []),
	lists:foreach(fun(X) -> erlbal_p:stop_server(Bal, X) end, erlbal_p:list_servers(Bal)),
	erlbal_p:stop_bal(Bal),
	ok;

start(args) ->
	Bal = erlbal_p:start_bal(),
	erlbal_p:start_server(Bal, node(), fun([X], _) -> {X+1, []} end, []),
	erlbal_p:start_server(Bal, node(), fun([X], _) -> {X+2, []} end, []),
	7 = erlbal_p:make_request(Bal, [6]),
	8 = erlbal_p:make_request(Bal, [6]),
	lists:foreach(fun(X) -> erlbal_p:stop_server(Bal, X) end, erlbal_p:list_servers(Bal)),
	erlbal_p:stop_bal(Bal),
	ok;

start(state) ->
	Bal = erlbal_p:start_bal(),
	erlbal_p:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S]} end, [1]),
	erlbal_p:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S]} end, [10]),
	7 = erlbal_p:make_request(Bal, [6]),
	16 = erlbal_p:make_request(Bal, [6]),
	lists:foreach(fun(X) -> erlbal_p:stop_server(Bal, X) end, erlbal_p:list_servers(Bal)),
	erlbal_p:stop_bal(Bal),
	ok;

start(complex_state) ->
	Bal = erlbal_p:start_bal(),
	erlbal_p:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S+1]} end, [1]),
	erlbal_p:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S+1]} end, [10]),
	7 = erlbal_p:make_request(Bal, [6]),
	16 = erlbal_p:make_request(Bal, [6]),
	8 = erlbal_p:make_request(Bal, [6]),
	17 = erlbal_p:make_request(Bal, [6]),
	lists:foreach(fun(X) -> erlbal_p:stop_server(Bal, X) end, erlbal_p:list_servers(Bal)),
	erlbal_p:stop_bal(Bal),
	ok.
