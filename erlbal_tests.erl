-module(erlbal_tests).
-export([start/1]).

start(simple) ->
	Bal = erlbal:start_bal(),
	erlbal:start_server(Bal, node(), fun(_, _) -> {7, []} end, []),
	erlbal:start_server(Bal, node(), fun(_, _) -> {8, []} end, []),
	7 = erlbal:make_request(Bal, []),
	8 = erlbal:make_request(Bal, []),
	erlbal:stop_bal(Bal),
	ok;

start(args) ->
	Bal = erlbal:start_bal(),
	erlbal:start_server(Bal, node(), fun([X], _) -> {X+1, []} end, []),
	erlbal:start_server(Bal, node(), fun([X], _) -> {X+2, []} end, []),
	7 = erlbal:make_request(Bal, [6]),
	8 = erlbal:make_request(Bal, [6]),
	erlbal:stop_bal(Bal),
	ok;

start(state) ->
	Bal = erlbal:start_bal(),
	erlbal:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S]} end, [1]),
	erlbal:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S]} end, [10]),
	7 = erlbal:make_request(Bal, [6]),
	16 = erlbal:make_request(Bal, [6]),
	erlbal:stop_bal(Bal),
	ok;

start(complex_state) ->
	Bal = erlbal:start_bal(),
	erlbal:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S+1]} end, [1]),
	erlbal:start_server(Bal, node(), fun([X], [S]) -> Y = X + S, {Y, [S+1]} end, [10]),
	7 = erlbal:make_request(Bal, [6]),
	16 = erlbal:make_request(Bal, [6]),
	8 = erlbal:make_request(Bal, [6]),
	17 = erlbal:make_request(Bal, [6]),
	erlbal:stop_bal(Bal),
	ok.
