-module (main).
-import(reader, [init/1]).
-import(node, [initNodes/3]).
-import(ant, [initAnts/5, initTechnicalAnt/3]).
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
	Nodes = initNodes(N, Adj, AntsQuantity),
	TechnicalAnt = initTechnicalAnt(erlang:monotonic_time(), AntsQuantity, Nodes),
    initAnts(TechnicalAnt, StopAfterSingleAntIterations, N, AntsQuantity, Nodes),
    TechnicalAnt ! {init, {rand:uniform(N)}},
    io:format("\n").

start() ->
	usage().

usage() ->
    io:format("usage: <problem path> <ants> <iterations>\n"),
    halt(1).


