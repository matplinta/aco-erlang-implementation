-module (main).
-import(reader, [init/1]).
-import(node, [initNodes/2, node/4]).
-import(ant, [initAnts/3, ant/3]).

-compile(export_all).

% start(FoodList) ->
% spawn(?MODULE, fridge2, [FoodList]).

init() -> 
    {N, _Map, Adj} = reader:init("../../problems/oliver30.tsp"),
	Nodes = initNodes(N, Adj),
	io:format("\n\nPIDS MAP\n~w", [Nodes]),
	initAnts(N, 10, Nodes).
