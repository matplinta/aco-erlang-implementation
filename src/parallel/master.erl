-module (master).
-export ([initMaster/3, master/6]).
-define(MAXINT, 9999999999999).

initMaster(StopCondition, AntsQt, TechnicalAnt) -> 
    spawn(master, master, [StopCondition, AntsQt, 0, TechnicalAnt, ?MAXINT, []]).

sendEvaporateOrder(TechnicalAnt, 0) -> 
    TechnicalAnt ! {evaporate}, 
    % io:format("\nEVAPORATEEEEEEEEEEEEEEE"),
    ok;
sendEvaporateOrder(_, _) -> nok.

master(StopCondition, AntsQt, AntsIter, TechnicalAnt, BestDistance, BestPath) ->
    sendEvaporateOrder(TechnicalAnt, AntsIter rem AntsQt),
    receive
        {check, {Ant, Distance, Path}} ->
            if 
                AntsIter/AntsQt >= StopCondition ->
                    Ant ! {die},
                    io:format("\nBEST FITNESS FOUND: ~w,\tPath: ~w", [BestDistance, BestPath]);
                    % master(StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
                Distance < BestDistance ->
                    io:format("\nNEW BEST FITNESS:  ~w,\tPath: ~w", [Distance, Path]),
                    master(StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, Distance, Path);
                true ->
                    master(StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
            end;
        _ ->
            master(StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
    end.
