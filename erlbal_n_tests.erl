-module(erlbal_n_tests).
-export([start/1]).

start(simple) ->
	Bal = erlbal_n:start_bal(),
	erlbal_n:start_server(Bal, node(), fun(_, _) -> {7, []} end, fun() -> 1000 - length(processes()) end, []),
	erlbal_n:start_server(Bal, node(), fun(_, _) -> {7, []} end, fun() -> 1000 - length(processes()) end, []),
	7 = erlbal_n:make_request(Bal, []),
	7 = erlbal_n:make_request(Bal, []),
	lists:foreach(fun(X) -> erlbal_n:stop_server(Bal, X) end, erlbal_n:list_servers(Bal)),
	erlbal_n:stop_bal(Bal),
	ok;

start(args) ->
	Bal = erlbal_n:start_bal(),
	erlbal_n:start_server(Bal, node(), fun([X], _) -> {X+1, []} end, fun() -> 1000 - length(processes()) end, []),
	erlbal_n:start_server(Bal, node(), fun([X], _) -> {X+1, []} end, fun() -> 1000 - length(processes()) end, []),
	7 = erlbal_n:make_request(Bal, [6]),
	7 = erlbal_n:make_request(Bal, [6]),
	lists:foreach(fun(X) -> erlbal_n:stop_server(Bal, X) end, erlbal_n:list_servers(Bal)),
	erlbal_n:stop_bal(Bal),
	ok;

start(state) ->
	Bal = erlbal_n:start_bal(),
	erlbal_n:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S]} end, fun() -> 1000 - length(processes()) end, [1]),
	erlbal_n:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S]} end, fun() -> 1000 - length(processes()) end, [1]),
	7 = erlbal_n:make_request(Bal, [6]),
	7 = erlbal_n:make_request(Bal, [6]),
	lists:foreach(fun(X) -> erlbal_n:stop_server(Bal, X) end, erlbal_n:list_servers(Bal)),
	erlbal_n:stop_bal(Bal),
	ok;

start(complex_state) ->
	Bal = erlbal_n:start_bal(),
	erlbal_n:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S+1]} end, fun() -> 1000 - length(processes()) end, [1]),
	erlbal_n:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S+1]} end, fun() -> 1000 - length(processes()) end, [1]),
	7 = erlbal_n:make_request(Bal, [6]),
	8 = erlbal_n:make_request(Bal, [6]),
	9 = erlbal_n:make_request(Bal, [6]),
	10 = erlbal_n:make_request(Bal, [6]),
	lists:foreach(fun(X) -> erlbal_n:stop_server(Bal, X) end, erlbal_n:list_servers(Bal)),
	erlbal_n:stop_bal(Bal),
	ok.
