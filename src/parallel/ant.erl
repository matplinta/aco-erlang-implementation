-module (ant).
-export ([initAnts/3, ant/3]).

initAnts(_N, 0, _NodesPids) -> ok;
initAnts(N, AntsQuantity, NodesPids) -> 
	Ant = spawn(ant, ant, [NodesPids, 0, []]),
	Ant ! {init, rand:uniform(N)},
	initAnts(N, AntsQuantity - 1, NodesPids).


ant(NodesPids, Distance, Path) -> 
    receive
        {init, StartNode} -> 
			NodePid = maps:get(StartNode, NodesPids),
			% io:format("\n ANT PID: ~w\n", [self()]),
			NodePid ! {self(), {where, [ StartNode | Path]}},
			ant(NodesPids, Distance, [ StartNode | Path]);
		{goto, 0, DistToFirst} ->
			self() ! {finish},
			ant(NodesPids, Distance + DistToFirst, lists:reverse([ lists:last(Path) | Path]));
		{goto, NextNode, DistToNode} ->
			NextNodePid = maps:get(NextNode, NodesPids),
			NextNodePid ! {self(), {where, [ NextNode | Path]}},
			
			ant(NodesPids, Distance + DistToNode, [ NextNode | Path]);
		{finish} -> 
			io:format("\nANT: Distance: ~w, PATH: ~w", [Distance, Path]);
		{die} ->
			exit(kill);
        _ ->
            ant(NodesPids, Distance, Path)
    end.