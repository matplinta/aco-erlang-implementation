-module (main).
-import(reader, [init/1]).
-import(node, [initNodes/2, node/4]).
-import(ant, [initAnts/4, ant/4]).
-import(master, [initMaster/0, master/2]).

-compile(export_all).

% start(FoodList) ->
% spawn(?MODULE, fridge2, [FoodList]).

init() -> 
	{N, _Map, Adj} = reader:init("../../problems/oliver30.tsp"),
	Master = initMaster(),
	Nodes = initNodes(N, Adj),
	io:format("\n\nPIDS MAP\n~w", [Nodes]),
	initAnts(Master, N, 2, Nodes).
