-module (master).
-export ([initMaster/0, master/2]).
-define(MAXINT, 9999999999999).

initMaster() -> 
    spawn(master, master, [?MAXINT, []]).

master(BestDistance, BestPath) ->
    receive
        {check, {Distance, Path}} ->
            if 
                Distance < BestDistance ->
                    io:format("\nNEW BEST FITNESS: ~w\n~w", [Distance, Path]),
                    master(Distance, Path);
                true ->
                    master(BestDistance, BestPath)
            end;
        _ ->
            master(BestDistance, BestPath)
    end.
