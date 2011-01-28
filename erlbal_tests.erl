-module(erlbal_tests).
-export([start/1]).

start(simple) ->
	Bal = erlbal:start_bal(),
	erlbal:start_server(Bal, node(), fun(_) -> 7 end),
	erlbal:start_server(Bal, node(), fun(_) -> 8 end),
	7 = erlbal:make_request(Bal, []),
	8 = erlbal:make_request(Bal, []),
	erlbal:stop_bal(Bal).
