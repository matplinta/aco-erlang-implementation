-module (main).
-import(reader, [init/1]).
-import(node, [initNodes/2, node/4]).
-import(ant, [initAnts/4, ant/4, initTechnicalAnt/1, technicalAnt/1]).
-import(master, [initMaster/3, master/6]).

-compile(export_all).

% start(FoodList) ->
% spawn(?MODULE, fridge2, [FoodList]).

init() -> 
	{N, _Map, Adj} = reader:init("../../problems/oliver30.tsp"),
	AntsQuantity = 30,
	StopAfterSingleAntIterations = 10000,

	Nodes = initNodes(N, Adj),
	TechnicalAnt = initTechnicalAnt(Nodes),
	Master = initMaster(StopAfterSingleAntIterations, AntsQuantity, TechnicalAnt),
	initAnts(Master, N, AntsQuantity, Nodes).
