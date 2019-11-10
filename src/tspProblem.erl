-module(tspProblem).
-import(tsplibReader, [init/1]).
-import(nearestNeighbour, [solve/2]).
-compile([export_all]).
-mode(compile).

evaluate(DistanceMap, Solution) -> evaluate(DistanceMap, Solution, 0).
evaluate(_DistanceMap, [], Acc) -> Acc;
evaluate(DistanceMap, [First, Second], Acc) ->
    Distance = maps:get({First, Second}, DistanceMap),
    evaluate(DistanceMap, [], Acc+Distance);
evaluate(DistanceMap, [First, Second | Rest], Acc) ->
    Distance = maps:get({First, Second}, DistanceMap),
    evaluate(DistanceMap, [Second | Rest], Acc+Distance).


initNodesToVisit(NumOfNodes, StartNode) ->
    NodesToVisit = [ X || X <- lists:seq(1, NumOfNodes), StartNode =/= X],
    NodesToVisit.

updateNodsToVisit([First | RestTour], []) ->
    case First =:= lists:last(RestTour) of 
        true -> [First];
        false -> []
    end;
updateNodsToVisit(_Tour, NodesToVisit) -> NodesToVisit.



main(_) ->
    {Dimension, _Map, DistanceMap} = init("/home/mateusz/Repos/aco-erlang-implementation/problems/oliver30.tsp"),
    Solution = solve(Dimension, DistanceMap),
    io:format("~w~n", [Solution]),
    Dist = evaluate(DistanceMap, Solution),
    io:format("~w", [Dist]).
