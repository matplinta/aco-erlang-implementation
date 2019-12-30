-module (master).
-export ([initMaster/3, master/7]).
-define(MAXINT, 9999999999999).

% INITIALIZE MASTER PROCESS
initMaster(StopCondition, AntsQt, TechnicalAnt) -> 
    spawn(master, master, [false, StopCondition, AntsQt, 1, TechnicalAnt, ?MAXINT, []]).

% HANDLE EVAPORATE CONDITION
sendEvaporateOrder(TechnicalAnt, 0) -> TechnicalAnt ! {evaporate}, ok;
sendEvaporateOrder(_, _) -> nok.

% MASTER PROCESS MESSAGE HANDLING
master(Stop, StopCondition, AntsQt, AntsIter, TechnicalAnt, BestDistance, BestPath) ->
    receive
        {check, {Ant, Distance, Path}} ->
            sendEvaporateOrder(TechnicalAnt, AntsIter rem AntsQt),
            if 
                AntsIter/AntsQt >= StopCondition ->
                    Ant ! {die},
                    case Stop of
                        false -> 
                            io:format("\nBEST PATH FOUND: ~w,\tPath: ~w", [BestDistance, BestPath]),
                            master(true, StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath);
                        true ->
                            master(Stop, StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
                    end;
                Distance < BestDistance ->
                    io:format("\nNew best path:  ~w,\tPath: ~w", [Distance, Path]),
                    master(Stop, StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, Distance, Path);
                true ->
                    master(Stop, StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
            end;
        _ ->
            master(Stop, StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
    end.
