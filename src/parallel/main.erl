-module (main).
-import(reader, [init/1]).
-import(node, [initNodes/2]).
-import(ant, [initAnts/4, initTechnicalAnt/1]).
-import(master, [initMaster/4]).
-export([run/0]).
% -compile(export_all).

run() -> 
	{N, _Map, Adj} = reader:init("../../problems/att532.tsp"),
	% {N, _Map, Adj} = reader:init("../../problems/oliver30.tsp"),
	AntsQuantity = 30,
	StopAfterSingleAntIterations = 100,

	Nodes = initNodes(N, Adj),
	TechnicalAnt = initTechnicalAnt(Nodes),
	Master = initMaster(erlang:monotonic_time(), StopAfterSingleAntIterations, AntsQuantity, TechnicalAnt),
	initAnts(Master, N, AntsQuantity, Nodes).
