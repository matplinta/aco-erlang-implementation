-module (node).
-export ([initNodes/2, node/4]).

% a parameter to control the influence of pheromones
-define(Alfa, 1).

% a parameter to control the influence the desirability of state transition
% typically 1 / dxy where d is the distance
-define(Beta, 1).

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

maxFromMap(Map) -> Iterator = maps:iterator(Map), maxFromMap(Iterator, 0, 0.0).
maxFromMap(Iterator, MaxKey, MaxVal) -> 
    case maps:next(Iterator) of
        {Key, Val, Iterrator2} ->
            if 
                Val >= MaxVal ->
                    maxFromMap(Iterrator2, Key, Val);
                true ->
                    maxFromMap(Iterrator2, MaxKey, MaxVal)
            end;
        none ->
            % io:format("MAX DISTANCE:~w ~w\n", [MaxKey, MaxVal]),
            MaxKey
    end.

whereTo(Probabilities, Visited) -> 
    Iterator = maps:iterator(Probabilities), 
    whereTo(Iterator, Visited, 0, 0.0).
whereTo(Iterator, Visited, MaxKey, MaxVal) -> 
    case maps:next(Iterator) of
        {Key, Val, Iterrator2} ->
            case lists:member(Key, Visited) of
                true -> 
                    whereTo(Iterrator2, Visited, MaxKey, MaxVal);
                false -> 
                    if 
                        Val >= MaxVal ->
                            % io:format("GTR, ~w: ~w\n", [Key, Visited]),
                            whereTo(Iterrator2, Visited, Key, Val);
                        true ->
                            % io:format("NOT GTR, ~w: ~w\n", [Key, Visited]),
                            whereTo(Iterrator2, Visited, MaxKey, MaxVal)
                    end
            end;
        none ->
            % io:format("MAX key, max val:~w ~w\n", [MaxKey, MaxVal]),
            MaxKey
    end.

countProbability(N, DistanceTo, Pheromones, CurrentNodeNo) -> countProbability(N, DistanceTo, Pheromones, CurrentNodeNo, #{}).
countProbability(0, DistanceTo, Pheromones, CurrentNodeNo, Acc) -> Acc;
countProbability(NodeNo, DistanceTo, Pheromones, CurrentNodeNo, Acc) ->
    if CurrentNodeNo == NodeNo ->
        countProbability(NodeNo - 1, DistanceTo, Pheromones, CurrentNodeNo, Acc);
        true -> 
            Distance = maps:get(NodeNo, DistanceTo),
            Pheromone = maps:get(NodeNo, Pheromones),
            Probability = probability(Distance, Pheromone),
            countProbability(NodeNo - 1, DistanceTo, Pheromones, CurrentNodeNo, maps:put(NodeNo, Probability, Acc))
    end.

% returns:	MAP #{NodeNo => PID}
% format:	DistanceTo, Pheromones: MAP #{NodeNo => value}
initNodes(N, AdjTable) -> initNodes(N, N, AdjTable, #{}).
initNodes(_N, 0, _AdjTable, Acc) -> Acc;
initNodes(N, NodeNo, AdjTable, Acc) -> 
	DistanceTo = initDistanceTo(N, NodeNo, AdjTable),
    Pheromones = initPheromones(N, NodeNo),
    Probability = countProbability(N, DistanceTo, Pheromones, NodeNo),
	io:format("Distance To, ~w: ~w\n", [NodeNo, DistanceTo]),
	io:format("Pheromones, ~w: ~w\n", [NodeNo, Pheromones]),
	io:format("Probability, ~w: ~w\n", [NodeNo, Probability]),
    Pid = spawn(node, node, [NodeNo, DistanceTo, Pheromones, Probability]),
    initNodes(N, NodeNo - 1, AdjTable, maps:put(NodeNo, Pid, Acc)).


    
node(NodeNo, DistanceTo, Pheromones, Probability) -> 
	receive
		{where, {Ant, Visited}} -> 
            % io:format("\n a czy tu wejde ~w, ~w\n", [Ant, Visited]),
            Node = whereTo(Probability, Visited),
            if 
                Node == 0 ->
                    Ant ! {goto, 0, maps:get(lists:last(Visited), DistanceTo)};
                true ->
                    Ant ! {goto, Node, maps:get(Node, DistanceTo)}
            end,
            node(NodeNo, DistanceTo, Pheromones, Probability);
        {update, {Next, Addition}} ->
            Value = maps:get(Next, Pheromones),
            UpdatedPheromones = maps:update(Next, Value + Addition, Pheromones),
            node(NodeNo, DistanceTo, UpdatedPheromones, Probability);
            
		_ ->
			node(NodeNo, DistanceTo, Pheromones, Probability)
	end.