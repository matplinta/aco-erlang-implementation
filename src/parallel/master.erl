-module (master).
-export ([initMaster/4, master/8]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%              MASTER              %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% INITIALIZE MASTER PROCESS
initMaster(StartTime, StopCondition, AntsQt, TechnicalAnt) -> 
    spawn(master, master, [StartTime, false, StopCondition, AntsQt, 1, TechnicalAnt, none, []]).

% HANDLE EVAPORATE CONDITION
sendEvaporateOrder(TechnicalAnt, 0) -> TechnicalAnt ! {evaporate}, ok;
sendEvaporateOrder(_, _) -> nok.

% MASTER PROCESS MESSAGE HANDLING
master(StartTime, Stop, StopCondition, AntsQt, AntsIter, TechnicalAnt, BestDistance, BestPath) ->
    receive
        {check, {Ant, Distance, Path}} ->
            % send evaporate message to every node only upon complete, on average, each ant iteration
            % meaning: if <every traversal(counting in every ant)> mod <ants quantity> == 0 => evaporate
            sendEvaporateOrder(TechnicalAnt, AntsIter rem AntsQt),
            if 
                % if there is only one ant left alive, kill it and tell Technical Ant to kill all nodes and itself also
                AntsQt == 1 ->
                    Ant ! {die},
                    TechnicalAnt ! {killnodes};
                % check for stop condition, if on average single ant traversed given number of iterations, send stop signal
                AntsIter/AntsQt >= StopCondition ->
                    Ant ! {die},
                    % case structure; only to display "BEST PATH FOUND" sign only once
                    case Stop of
                        false -> 
                            Time = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, millisecond),
                            io:format("\n\nBEST PATH FOUND: ~w,\tPath: ~w", [BestDistance, BestPath]),
                            io:format("\nTIME ELAPSED:\t~w ms\n", [Time]),
                            master(StartTime, true, StopCondition, AntsQt - 1, AntsIter + 1, TechnicalAnt, BestDistance, BestPath);
                        true ->
                            master(StartTime, Stop, StopCondition, AntsQt - 1, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
                    end;
                % check for a better path
                Distance < BestDistance ->
                    io:format("\nNew best path:  ~w,\tPath: ~w", [Distance, Path]),
                    master(StartTime, Stop, StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, Distance, Path);
                true ->
                    master(StartTime, Stop, StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
            end;
        _ ->
            master(StartTime, Stop, StopCondition, AntsQt, AntsIter + 1, TechnicalAnt, BestDistance, BestPath)
    end.
