-module (node).
-export ([initNodes/2, node/3]).

% evaporation coefficient
-define(EvaCo, 0.05).

% INITIALIZE PHEROMONES MAP
initPheromones(N, Omit) -> initPheromones(N, Omit, N, #{}).
initPheromones(_, _, 0, Acc) -> Acc;
initPheromones(N, Omit, NodeNo, Acc) -> 
	if 
		Omit == NodeNo ->
			initPheromones(N, Omit, NodeNo - 1, Acc);
		true ->
			initPheromones(N, Omit, NodeNo - 1, maps:put(NodeNo, 1.0, Acc))
	end.

% INITIALIZE DISTANCETO MAP OF DISTANCES TO OTHER NODES
initDistanceTo(N, Omit, AdjTable) -> initDistanceTo(N, Omit, AdjTable, N, #{}).
initDistanceTo(_, _, _, 0, Acc) -> Acc;
initDistanceTo(N, Omit, AdjTable, NodeNo, Acc) -> 
	if 
		Omit == NodeNo ->
			initDistanceTo(N, Omit, AdjTable, NodeNo - 1, Acc);
		true ->
			Distance = maps:get({Omit,NodeNo}, AdjTable),
			initDistanceTo(N, Omit, AdjTable, NodeNo - 1, maps:put(NodeNo, Distance, Acc))
	end.

% INITIALIZE NODES
% 	RETURNS #{NodeNo => PID, ...}
%   FORMAT:	DistanceTo, Pheromones: #{NodeNo => value, ...}
initNodes(N, AdjTable) -> initNodes(N, N, AdjTable, #{}).
initNodes(_N, 0, _AdjTable, Acc) -> Acc;
initNodes(N, NodeNo, AdjTable, Acc) -> 
	DistanceTo = initDistanceTo(N, NodeNo, AdjTable),
    Pheromones = initPheromones(N, NodeNo),
    % Probability = countProbability(DistanceTo, Pheromones, NodeNo),
    Pid = spawn(node, node, [NodeNo, DistanceTo, Pheromones]),
    initNodes(N, NodeNo - 1, AdjTable, maps:put(NodeNo, Pid, Acc)).
    
% NODE PROCESS MESSAGE HANDLING
node(NodeNo, DistanceTo, Pheromones) -> 
	receive
		{where, {Ant}} -> 
            Ant ! {decide, {DistanceTo, Pheromones}},
            node(NodeNo, DistanceTo, Pheromones);

        {update, {Next, Addition}} ->
            % Start = erlang:monotonic_time(),

            NewPheromoneValue = maps:get(Next, Pheromones) + Addition,
            UpdatedPheromones = maps:update(Next, NewPheromoneValue, Pheromones),
            % Distance = maps:get(Next, DistanceTo),
            % UpdatedProbability = maps:update(Next, probability(Distance, NewPheromoneValue), Probability),
            % io:format("Node: ~w, PHEROMONE UPDATE BEFORE: ~w\n~w\n", [NodeNo, Pheromones, Probability]),
            % io:format("Node: ~w, PHEROMONE UPDATE AFTER: ~w\n~w\n", [NodeNo, UpdatedPheromones, UpdatedProbability]),
            % Stop = erlang:monotonic_time(),
            % io:format("UPDATE: ~w\t", [Stop-Start]),

			node(NodeNo, DistanceTo, UpdatedPheromones);
			
        {evaporate} ->
            Evaporate = fun(K, V, Acc) -> maps:put(K, (1 - ?EvaCo) * V, Acc) end,

            % io:format("Node: ~w, BEFORE EVAPOR: ~w\n", [NodeNo, Pheromones]),

            EvaporatedPheromones = maps:fold(Evaporate, #{}, Pheromones),

            % io:format("Node: ~w, AFTER EVAPOR: ~w\n", [NodeNo, EvaporatedPheromones]),
            
            % UpdatedProbability = countProbability(DistanceTo, EvaporatedPheromones, NodeNo),
            node(NodeNo, DistanceTo, EvaporatedPheromones);
        {die} ->
                io:format("NODE: I was killed :(\n"),
                exit(kill);
		_ ->
			node(NodeNo, DistanceTo, Pheromones)
	end.