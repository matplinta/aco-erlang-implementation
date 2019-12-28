-module (ant).
-export ([initAnts/4, ant/4]).

% Pheromone level deposition constant; Pheromone deposited on a node = Q/Lk
% where Lk is a cost of the k'th ant tour, typically, in our case, lenght of the tour
-define(Q, 20).

initAnts(_Master, _N, 0, _NodesPids) -> ok;
initAnts(Master, N, AntsQuantity, NodesPids) -> 
	Ant = spawn(ant, ant, [Master, NodesPids, 0, []]),
	Ant ! {init, rand:uniform(N)},
	initAnts(Master, N, AntsQuantity - 1, NodesPids).

updatePheromonesOnPath(_, _, [ _ | []]) -> ok;
updatePheromonesOnPath(NodesPids, Addition, [ First | [ Next | _ ] = Path ] ) ->
    Pid = maps:get(First, NodesPids),
    Pid ! {update, {Next, Addition}},
    % io:format("\nPath\n~w", [Path]),
    updatePheromonesOnPath(NodesPids, Addition, Path).

ant(Master, NodesPids, Distance, Path) -> 
    receive
        {init, StartNode} -> 
			NodePid = maps:get(StartNode, NodesPids),
			% io:format("\n ANT PID: ~w\n", [self()]),
			NodePid ! {where, {self(), [ StartNode | Path]}},
			ant(Master, NodesPids, Distance, [ StartNode | Path]);
		{goto, 0, DistToFirst} ->
			self() ! {finish},
			ant(Master, NodesPids, Distance + DistToFirst, lists:reverse([ lists:last(Path) | Path]));
		{goto, NextNode, DistToNode} ->
			NextNodePid = maps:get(NextNode, NodesPids),
			NextNodePid ! {where, {self(), [ NextNode | Path]}},
			ant(Master, NodesPids, Distance + DistToNode, [ NextNode | Path]);
		{finish} -> 
            % io:format("\nANT: Distance: ~w, PATH: ~w", [Distance, Path]),
            % send to master info of best fitness
            Master ! {check, {Distance, Path}},
            % update pheromone table on path
            Addition = ?Q / Distance,
            updatePheromonesOnPath(NodesPids, Addition, Path),
            % restart ant's journey
            self() ! {init, rand:uniform(maps:size(NodesPids))},
            ant(Master, NodesPids, 0, []);
		{die} ->
			exit(kill);
        _ ->
            ant(Master, NodesPids, Distance, Path)
    end.