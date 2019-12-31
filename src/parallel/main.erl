-module (main).
-import(reader, [init/1]).
-import(node, [initNodes/2]).
-import(ant, [initAnts/4, initTechnicalAnt/1]).
-import(master, [initMaster/4]).
-export([start/1, start/0]).
% -compile(export_all).

start([ProblemPath, Ants, Iterations | _]) -> 
	% {N, _Map, Adj} = reader:init("../problems/att532.tsp"),
	% % {N, _Map, Adj} = reader:init("../../problems/oliver30.tsp"),
	% AntsQuantity = 30,
	% StopAfterSingleAntIterations = 100,

	{N, _Map, Adj} = reader:init(erlang:atom_to_list(ProblemPath)),
	AntsQuantity = erlang:list_to_integer(erlang:atom_to_list(Ants)),
	StopAfterSingleAntIterations = erlang:list_to_integer(erlang:atom_to_list(Iterations)),
	Nodes = initNodes(N, Adj),
	TechnicalAnt = initTechnicalAnt(Nodes),
	Master = initMaster(erlang:monotonic_time(), StopAfterSingleAntIterations, AntsQuantity, TechnicalAnt),
	initAnts(Master, N, AntsQuantity, Nodes).

start() ->
	usage().

usage() ->
    io:format("usage: <problem path> <ants> <iterations>\n"),
    halt(1).


