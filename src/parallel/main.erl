-module (main).
-import(reader, [init/1]).
-import(node, [initNodes/2, node/4]).
-import(ant, [initAnts/4, ant/4, initTechnicalAnt/1, technicalAnt/1]).
-import(master, [initMaster/3, master/6]).
-export([run/0]).
% -compile(export_all).

run() -> 
	{N, _Map, Adj} = reader:init("../../problems/oliver6.tsp"),
	AntsQuantity = 1,
	StopAfterSingleAntIterations = 2,

	Nodes = initNodes(N, Adj),
	TechnicalAnt = initTechnicalAnt(Nodes),
	Master = initMaster(StopAfterSingleAntIterations, AntsQuantity, TechnicalAnt),
	initAnts(Master, N, AntsQuantity, Nodes).
