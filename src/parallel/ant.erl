-module (ant).
-export ([initAnts/5, ant/5, initTechnicalAnt/3, technicalAnt/7]).

% Pheromone level deposition constant; Pheromone deposited on a node = Q/Lk
% where Lk is a cost of the k'th ant tour, typically, in our case, lenght of the tour
-define(Q, 210).

% a parameter to control the influence of pheromones
-define(Alfa, 1).

% a parameter to control the influence the desirability of state transition
% typically 1 / dxy where d is the distance
-define(Beta, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%                ANT                %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% INITIALIZE ANTS
initAnts(_Master, _Iterations, _N, 0, _NodesPids) -> ok;
initAnts(TechnicalAnt, Iterations, N, AntsQuantity, NodesPids) -> 
	Ant = spawn(ant, ant, [TechnicalAnt, Iterations, NodesPids, 0, []]),
	Ant ! {init, {rand:uniform(N)}},
	initAnts(TechnicalAnt, Iterations, N, AntsQuantity - 1, NodesPids).

% UPDATE PHEROMONE LEVEL ON TRAVERSED PATH
updatePheromonesOnPath(_, _, [ _ | []]) -> ok;
updatePheromonesOnPath(NodesPids, Addition, [ First | [ Next | _ ] = Path ] ) ->
    FirstPid = maps:get(First, NodesPids),
    NextPid = maps:get(Next, NodesPids),
    FirstPid ! {update, {Next, Addition}},
    NextPid ! {update, {First, Addition}},
    updatePheromonesOnPath(NodesPids, Addition, Path).

% SUM VALUES IN A MAP
sumMap(Map) ->
    lists:sum(maps:values(Map)).

% COUNT PROBABILITY OF GIVEN EDGE
probabilityUnnormalized(Distance, Pheromone) ->
    math:pow(Pheromone, ?Alfa) * math:pow(1 / Distance, ?Beta).

% COUNT PROBABILITY MAP OF ALL PASSED NODES - EDGES
% ASSUMPTION: Keys of DistanceTo are equal to those of Pheromones map
countProbability(DistanceTo, Pheromones) -> 
    countProbability(maps:keys(DistanceTo), DistanceTo, Pheromones, #{}).
countProbability([], _, _, Acc) -> 
	Sum = sumMap(Acc),
	Normalize = fun(K, V, Axx) -> maps:put(K, V/Sum, Axx) end,
	NormalizedProbabilities = maps:fold(Normalize, #{}, Acc),
	NormalizedProbabilities;
countProbability([NodeNo | Nodes], DistanceTo, Pheromones, Acc) ->
    Distance = maps:get(NodeNo, DistanceTo),
    Pheromone = maps:get(NodeNo, Pheromones),
    Probability = probabilityUnnormalized(Distance, Pheromone),
	countProbability(Nodes, DistanceTo, Pheromones, maps:put(NodeNo, Probability, Acc)).
	
% ROULETTE WHEEL SELECTION METHOD
rouletteWheel(Probabilities, SumOf) -> 
	Nodes = maps:keys(Probabilities),
	[First | _] = Nodes,
	rouletteWheel(Probabilities, rand:uniform() * SumOf , Nodes, maps:get(First, Probabilities)).
rouletteWheel(_, _, [Last], _) -> Last;
rouletteWheel(Probabilities, Rand, [First | [Second | _] = ReducedNodes], Prob) ->
	case Prob < Rand of
		true -> 
			NewProb = Prob + maps:get(Second, Probabilities),
			rouletteWheel(Probabilities, Rand, ReducedNodes, NewProb);
		false ->
			First
	end.

% SELECT NEXT NODE BASED ON PROBABILITIES USING ROULETTE WHEEL SELECTION
selectNextNode(DistanceTo, Pheromones, Visited) ->
    DistanceToOfNTV = maps:without(Visited, DistanceTo),
    PheromonesOfNTV = maps:without(Visited, Pheromones),
    ProbabilitiesOfNTV = countProbability(DistanceToOfNTV, PheromonesOfNTV),
	SumOfProbabilities = sumMap(ProbabilitiesOfNTV),
    rouletteWheel(ProbabilitiesOfNTV, SumOfProbabilities).

% ANT PROCESS MESSAGE HANDLING
ant(TechnicalAnt, Iterations, NodesPids, Distance, Path) -> 
    receive
        {init, {StartNode}} -> 
			NodePid = maps:get(StartNode, NodesPids),
			NodePid ! {where, {self()}},
			ant(TechnicalAnt, Iterations, NodesPids, Distance, [ StartNode | Path]);
        {decide, {DistanceTo, Pheromones}} -> 
			case maps:size(NodesPids) == length(Path) of
				true ->
					FirstNode = lists:last(Path),
                    DistToFirst = maps:get(FirstNode, DistanceTo),
                    self() ! {finish},
					ant(TechnicalAnt, Iterations, NodesPids, Distance + DistToFirst, lists:reverse([ FirstNode | Path]));
				false ->
            		NextNode = selectNextNode(DistanceTo, Pheromones, Path),
                    NextNodePid = maps:get(NextNode, NodesPids),
                    NextNodePid ! {where, {self()}},
                    DistToNode = maps:get(NextNode, DistanceTo),
                    ant(TechnicalAnt, Iterations, NodesPids, Distance + DistToNode, [ NextNode | Path])
            end;
		{finish} -> 
			% UNCOMMENT IF YOU WISH TO SEE EVERY SINGLE ANT'S DISTANCE AND PATH
			% io:format("\nAnt: Distance: ~w,\tPath: ~w", [Distance, Path]),
            % update pheromone table on path
            Addition = ?Q / Distance,
            updatePheromonesOnPath(NodesPids, Addition, Path),
            % restart ant's journey
            self() ! {init, {rand:uniform(maps:size(NodesPids))}},
            if
                Iterations == 0 -> 
                    TechnicalAnt ! {antdied},
                    exit(kill);
                true -> ant(TechnicalAnt, Iterations - 1, NodesPids, 0, [])
            end;
		% {die} ->
        %     io:format("I die\n"),
		% 	exit(kill);
        _ ->
            ant(TechnicalAnt, Iterations, NodesPids, Distance, Path)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%           TECHNICAL ANT           %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterate(Size, Iterator, MaxKey, MaxVal) -> 
    case maps:next(Iterator) of
        {Key, Val, Iterrator2} ->
            if 
                Size == 0 ->
                    Key;
                Val >= MaxVal ->
                    iterate(Size, Iterrator2, Key, Val);
                true ->
                    iterate(Size, Iterrator2, MaxKey, MaxVal)
            end;
        none ->
            MaxKey
    end.

% SELECT NEXT NODE BASED ON PROBABILITIES BY HIGHEST VALUE
selectNextNodeByHighestValue(DistanceTo, Pheromones, Visited) -> 
    DistanceToOfNTV = maps:without(Visited, DistanceTo),
    PheromonesOfNTV = maps:without(Visited, Pheromones),
    ProbabilitiesOfNTV = countProbability(DistanceToOfNTV, PheromonesOfNTV),
    Iterator = maps:iterator(ProbabilitiesOfNTV), 
    iterate(maps:size(ProbabilitiesOfNTV), Iterator, 0, 0.0).



% DISTRIBUTE GIVEN MESSAGE TO ALL NODES METHOD
distributeToNodes(NodesPids, Msg) -> distributeToNodes(NodesPids, Msg, maps:next(maps:iterator(NodesPids))).
distributeToNodes(_, _, none) -> ok;
distributeToNodes(NodesPids, Msg, {_, NodePid, NewIterator}) ->
    NodePid ! {Msg},
	distributeToNodes(NodesPids, Msg, maps:next(NewIterator)).

% INITIALIZE TECHNICAL ANT
initTechnicalAnt(StartTime, AntsQuantity, NodesPids) -> 
    spawn(ant, technicalAnt, [StartTime, AntsQuantity, NodesPids, 0, [], none, []]).

% TECHNICAL ANT PROCESS MESSAGE HANDLING
technicalAnt(StartTime, AntsQuantity, NodesPids, Distance, Path, BestDistance, BestPath) -> 
    receive
        {init, {StartNode}} -> 
            NodePid = maps:get(StartNode, NodesPids),
            NodePid ! {where, {self()}},
            technicalAnt(StartTime, AntsQuantity, NodesPids, Distance, [ StartNode | Path], BestDistance, BestPath);
        {decide, {DistanceTo, Pheromones}} -> 
            case maps:size(NodesPids) == length(Path) of
                true ->
                    FirstNode = lists:last(Path),
                    DistToFirst = maps:get(FirstNode, DistanceTo),
                    self() ! {finish},
                    technicalAnt(StartTime, AntsQuantity, NodesPids, Distance + DistToFirst, lists:reverse([ FirstNode | Path]), BestDistance, BestPath);
                false ->
                    NextNode = selectNextNodeByHighestValue(DistanceTo, Pheromones, Path),
                    NextNodePid = maps:get(NextNode, NodesPids),
                    NextNodePid ! {where, {self()}},
                    DistToNode = maps:get(NextNode, DistanceTo),
                    technicalAnt(StartTime, AntsQuantity, NodesPids, Distance + DistToNode, [ NextNode | Path], BestDistance, BestPath)
                end;
        {finish} -> 
            % UNCOMMENT IF YOU WISH TO SEE EVERY SINGLE ANT'S DISTANCE AND PATH
            % io:format("Ant: Distance: ~w,\tPath: ~w\n", [Distance, Path]),
            Time = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, millisecond),
            if 
                Distance < BestDistance ->
                    io:format("New best path: ~s,\tFound after: ~w ms\n", [float_to_list(Distance,[{decimals,3},compact]) , Time]),
                    % restart ant's journey
                    self() ! {init, {rand:uniform(maps:size(NodesPids))}},
                    technicalAnt(StartTime, AntsQuantity, NodesPids, 0, [], Distance, Path);
                AntsQuantity == 0 ->
                    io:format("------------------------------------------------------------------------------------------\n"),
                    io:format("BEST PATH FOUND: ~s,\n", [float_to_list(BestDistance,[{decimals,3},compact])]),
                    io:format("TIME ELAPSED: ~w ms\n", [Time]),
                    io:format("Path: ~w\n", [BestPath]),
                    self() ! {killnodes},
                    technicalAnt(StartTime, AntsQuantity, NodesPids, 0, [], BestDistance, BestPath);
                true ->
                    % restart ant's journey
                    self() ! {init, {rand:uniform(maps:size(NodesPids))}},
                    technicalAnt(StartTime, AntsQuantity, NodesPids, 0, [], BestDistance, BestPath)
            end;
            
        {killnodes} ->
            % kill all nodes and also technical ant exits (dies :c)
            distributeToNodes(NodesPids, die),
            % io:format("Ants, Nodes and TechnicalAnt exited.\n"),
            halt(0);
        {antdied} ->
            technicalAnt(StartTime, AntsQuantity - 1, NodesPids, Distance, Path, BestDistance, BestPath);
        _ ->
            technicalAnt(StartTime, AntsQuantity, NodesPids, Distance, Path, BestDistance, BestPath)
    end.