-module (node).
-export ([initNodes/2, node/4]).

% a parameter to control the influence of pheromones
-define(Alfa, 1).

% a parameter to control the influence the desirability of state transition
% typically 1 / dxy where d is the distance
-define(Beta, 1).

% evaporation coefficient
-define(EvaCo, 0.16).

initPheromones(N, Omit) -> initPheromones(N, Omit, N, #{}).
initPheromones(N, Omit, 0, Acc) -> Acc;
initPheromones(N, Omit, NodeNo, Acc) -> 
	if 
		Omit == NodeNo ->
			initPheromones(N, Omit, NodeNo - 1, Acc);
		true ->
			initPheromones(N, Omit, NodeNo - 1, maps:put(NodeNo, 1.0, Acc))
	end.

initDistanceTo(N, Omit, AdjTable) -> initDistanceTo(N, Omit, AdjTable, N, #{}).
initDistanceTo(N, Omit, AdjTable, 0, Acc) -> Acc;
initDistanceTo(N, Omit, AdjTable, NodeNo, Acc) -> 
	if 
		Omit == NodeNo ->
			initDistanceTo(N, Omit, AdjTable, NodeNo - 1, Acc);
		true ->
			Distance = maps:get({Omit,NodeNo}, AdjTable),
			initDistanceTo(N, Omit, AdjTable, NodeNo - 1, maps:put(NodeNo, Distance, Acc))
	end.

probability(Distance, Pheromone) ->
    math:pow(Pheromone, ?Alfa) * math:pow(1 / Distance, ?Beta).

countProbability(DistanceTo, Pheromones, CurrentNodeNo) -> 
    countProbability(maps:keys(DistanceTo), DistanceTo, Pheromones, CurrentNodeNo, #{}).
countProbability([], _, _, _, Acc) -> Acc;
countProbability([NodeNo | Nodes], DistanceTo, Pheromones, CurrentNodeNo, Acc) ->
    Distance = maps:get(NodeNo, DistanceTo),
    Pheromone = maps:get(NodeNo, Pheromones),
    Probability = probability(Distance, Pheromone),
    countProbability(Nodes, DistanceTo, Pheromones, CurrentNodeNo, maps:put(NodeNo, Probability, Acc)).

% returns:	MAP #{NodeNo => PID}
% format:	DistanceTo, Pheromones: MAP #{NodeNo => value}
initNodes(N, AdjTable) -> initNodes(N, N, AdjTable, #{}).
initNodes(_N, 0, _AdjTable, Acc) -> Acc;
initNodes(N, NodeNo, AdjTable, Acc) -> 
	DistanceTo = initDistanceTo(N, NodeNo, AdjTable),
    Pheromones = initPheromones(N, NodeNo),
    Probability = countProbability(DistanceTo, Pheromones, NodeNo),
	% io:format("Distance To, ~w: ~w\n", [NodeNo, DistanceTo]),
	% io:format("Pheromones, ~w: ~w\n", [NodeNo, Pheromones]),
	% io:format("Probability, ~w: ~w\n", [NodeNo, Probability]),
    Pid = spawn(node, node, [NodeNo, DistanceTo, Pheromones, Probability]),
    initNodes(N, NodeNo - 1, AdjTable, maps:put(NodeNo, Pid, Acc)).
    
node(NodeNo, DistanceTo, Pheromones, Probability) -> 
	receive
		{where, {Ant}} -> 
            Ant ! {decide, {DistanceTo, Probability}},
            node(NodeNo, DistanceTo, Pheromones, Probability);

        {update, {Next, Addition}} ->
            % Start = erlang:monotonic_time(),

            NewPheromoneValue = maps:get(Next, Pheromones) + Addition,
            UpdatedPheromones = maps:update(Next, NewPheromoneValue, Pheromones),
            Distance = maps:get(Next, DistanceTo),
            UpdatedProbability = maps:update(Next, probability(Distance, NewPheromoneValue), Probability),

            % Stop = erlang:monotonic_time(),
            % io:format("UPDATE: ~w\t", [Stop-Start]),

            node(NodeNo, DistanceTo, UpdatedPheromones, UpdatedProbability);

        {evaporate} ->
            Evaporate = fun(K, V, Acc) -> maps:put(K, (1 - ?EvaCo) * V, Acc) end,

            % io:format("\nNode: ~w, BEFORE EVAPOR: ~w\t\n", [NodeNo, Pheromones]),

            EvaporatedPheromones = maps:fold(Evaporate, #{}, Pheromones),

            % io:format("\nNode: ~w, AFTER EVAPOR: ~w\t\n", [NodeNo, EvaporatedPheromones]),
            
            UpdatedProbability = countProbability(DistanceTo, EvaporatedPheromones, NodeNo),
            node(NodeNo, DistanceTo, EvaporatedPheromones, UpdatedProbability);
        {die} ->
                io:format("NODE: I was killed :(\n"),
                exit(kill);
		_ ->
			node(NodeNo, DistanceTo, Pheromones, Probability)
	end.