-module (main).
-import(reader, [init/1]).
-import(node, [initNodes/2, node/3]).
-import(ant, [initAnts/4, ant/4, initTechnicalAnt/1, technicalAnt/1]).
-import(master, [initMaster/4, master/8]).
-export([run/0]).
% -compile(export_all).

run() -> 
	{N, _Map, Adj} = reader:init("../../problems/oliver30.tsp"),
	AntsQuantity = 30,
	StopAfterSingleAntIterations = 1000,

	Nodes = initNodes(N, Adj),
	TechnicalAnt = initTechnicalAnt(Nodes),
	Master = initMaster(erlang:monotonic_time(), StopAfterSingleAntIterations, AntsQuantity, TechnicalAnt),
	initAnts(Master, N, AntsQuantity, Nodes).
