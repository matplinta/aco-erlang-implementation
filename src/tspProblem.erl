-module(tspProblem).
-import(tsplibReader, [init/1]).
-import(nearestNeighbour, [solve/2]).
% -compile([export_all]).
-mode(compile).

main(_) ->
    {Dimension, _Map, Distance} = init("/home/mateusz/Repos/aco-erlang-implementation/problems/oliver30.tsp"),
    solve(Dimension, Distance).
