-module (ant).
-export ([initAnts/4, ant/4, initTechnicalAnt/1, technicalAnt/1]).

% Pheromone level deposition constant; Pheromone deposited on a node = Q/Lk
% where Lk is a cost of the k'th ant tour, typically, in our case, lenght of the tour
-define(Q, 250).

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
                            whereTo(Iterrator2, Visited, Key, Val);
                        true ->
                            whereTo(Iterrator2, Visited, MaxKey, MaxVal)
                    end
            end;
        none ->
            MaxKey
    end.

% PRETTIER BUT MORE TIME-COSTLY
% findMax(Key, Value, {undefined, undefined}) -> {Key, Value};
% findMax(Key, Value, {_, Value2}) when Value > Value2 -> {Key, Value};
% findMax(_Key, _Value, Acc) -> Acc.

% whereTo(Probabilities, Visited) ->
%     NotVisitedNodesProbMap = maps:without(Visited, Probabilities),
%     {BestNextNode, _} = maps:fold(fun findMax/3, {undefined, undefined}, NotVisitedNodesProbMap),
%     % io:format("Probabilities before: ~w\n", [Probabilities]),
%     % io:format("Probabilities after: ~w\n", [NotVisitedNodesProbMap]),
%     % io:format("Output: ~w\n", [{BestNextNode, A}]),
%     case BestNextNode of 
%         undefined -> 0;
%         _ -> BestNextNode
%     end.



ant(Master, NodesPids, Distance, Path) -> 
    receive
        {init, StartNode} -> 
			NodePid = maps:get(StartNode, NodesPids),
			% io:format("\n ANT PID: ~w\n", [self()]),
			NodePid ! {where, {self()}},
			ant(Master, NodesPids, Distance, [ StartNode | Path]);
		% {goto, 0, DistToFirst} ->
		% 	self() ! {finish},
		% 	ant(Master, NodesPids, Distance + DistToFirst, lists:reverse([ lists:last(Path) | Path]));
		% {goto, NextNode, DistToNode} ->
		% 	NextNodePid = maps:get(NextNode, NodesPids),
		% 	NextNodePid ! {where, {self(), [ NextNode | Path]}},
        %     ant(Master, NodesPids, Distance + DistToNode, [ NextNode | Path]);

        {decide, {DistanceTo, Probabilities}} -> 
            NextNode = whereTo(Probabilities, Path),
            if 
                NextNode == 0 ->
                    FirstNode = lists:last(Path),
                    DistToFirst = maps:get(FirstNode, DistanceTo),
                    self() ! {finish},
                    ant(Master, NodesPids, Distance + DistToFirst, lists:reverse([ FirstNode | Path]));
                true ->
                    NextNodePid = maps:get(NextNode, NodesPids),
                    NextNodePid ! {where, {self()}},
                    DistToNode = maps:get(NextNode, DistanceTo),
                    ant(Master, NodesPids, Distance + DistToNode, [ NextNode | Path])
            end;

		{finish} -> 
            % io:format("\nANT: Distance: ~w, PATH: ~w", [Distance, Path]),
            % send to master info of best fitness
            Master ! {check, {self(), Distance, Path}},
            % update pheromone table on path
            Addition = ?Q / Distance,
            updatePheromonesOnPath(NodesPids, Addition, Path),
            % restart ant's journey
            self() ! {init, rand:uniform(maps:size(NodesPids))},
            ant(Master, NodesPids, 0, []);
		{die} ->
            io:format("\nANT: I die :(\n"),
			exit(kill);
        _ ->
            ant(Master, NodesPids, Distance, Path)
    end.

evaporateAllNodes(NodesPids) -> 
    Iterator = maps:iterator(NodesPids),
    evaporateAllNodes(NodesPids, maps:next(Iterator)).
evaporateAllNodes(NodesPids, none) -> ok;
evaporateAllNodes(NodesPids, {_, NodePid, NewIterator}) ->
    NodePid ! {evaporate},
    evaporateAllNodes(NodesPids, maps:next(NewIterator)).

initTechnicalAnt(NodesPids) -> 
    spawn(ant, technicalAnt, [NodesPids]).

technicalAnt(NodesPids) ->
    receive
        {evaporate} ->
            evaporateAllNodes(NodesPids),
            technicalAnt(NodesPids);
        _ ->
            technicalAnt(NodesPids)
    end.


