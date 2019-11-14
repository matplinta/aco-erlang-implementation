-module(main).
-import(tsplibReader, [init/1]).
-compile(export_all).

main_loop(Iterations, NumberOfAnts, NumberOfNodes, Distances) -> 
  Pheromones = initialize_pheromones(NumberOfNodes),
  main_loop(Iterations, NumberOfAnts, NumberOfNodes, Distances, Pheromones).
main_loop(0, NumberOfAnts, NumberOfNodes, Distances, Pheromones) ->
  {Pheromones, Distances};
main_loop(Iterations, NumberOfAnts, NumberOfNodes, Distances, Pheromones) ->
	% io:format("Iterations left: ~w~n", [Iterations]),
  AntsPaths = ant_loop(NumberOfAnts, NumberOfNodes, Distances, Pheromones),
  NewPheromones = update_pheromones(Pheromones, AntsPaths),
	main_loop(Iterations - 1, NumberOfAnts, NumberOfNodes, Distances, NewPheromones).

initialize_pheromones(NumberOfNodes) ->
  Edges = [{X, Y} || X <- lists:seq(1, NumberOfNodes), Y <- lists:seq(1, NumberOfNodes)],
  Fun = fun(Elem, Map) -> maps:put(Elem, 1.0, Map) end,
  lists:foldl(Fun, #{}, Edges).

ant_loop(NumberOfAnts, NumberOfNodes, Distances, Pheromones) -> ant_loop(NumberOfAnts, NumberOfNodes, Distances, Pheromones, []).
ant_loop(0, NumberOfNodes, Distances, Pheromones, AntsPaths) -> AntsPaths;
ant_loop(NumberOfAnts, NumberOfNodes, Distances, Pheromones, AntsPaths) ->
  % io:format("Ants left: ~w~n", [NumberOfAnts]),
  NodesToVisit = [X || X <- lists:seq(1, NumberOfNodes)],
  % io:format("Nodes to visit: ~w~n", [NodesToVisit]),
	Path = get_path(Distances, Pheromones, NodesToVisit),
  % NewBestPath = better_path(Path, CurrentBestPath),
	ant_loop(NumberOfAnts - 1, NumberOfNodes, Distances, Pheromones, [Path|AntsPaths]).

update_pheromones(Pheromones, [FirstAntPath|AntsPaths]) -> 
  DecayedPheromones = decay_all_edges(Pheromones),
  update_pheromones_of_ants(DecayedPheromones, AntsPaths).
update_pheromones_of_ants(Pheromones, [{Nodes, Dist, Moves}]) -> update_pheromones_of_moves(Pheromones, Dist, Moves);
update_pheromones_of_ants(Pheromones, [CurrentAntPath|AntsPathsToUpdate]) ->
  {Nodes, Dist, Moves} = CurrentAntPath,
  UpdatedPheromones = update_pheromones_of_moves(Pheromones, Dist, Moves),
  update_pheromones_of_ants(UpdatedPheromones, AntsPathsToUpdate).
  
update_pheromones_of_moves(Pheromones, Dist, [LastMove]) -> update_pheromone_of_move(LastMove, Dist, Pheromones);
update_pheromones_of_moves(Pheromones, Dist, [CurrentMove|RestMoves]) ->
  UpdatedPheromones = update_pheromone_of_move(CurrentMove, Dist, Pheromones),
  update_pheromones_of_moves(UpdatedPheromones, Dist, RestMoves).

update_pheromone_of_move(Move, Dist, Pheromones) ->
  Fun = fun(Pher) -> 
    Pher + 1/Dist
    end,
  maps:update_with(Move, Fun, Pheromones).

decay_all_edges(Pheromones) -> 
  Ro = 0.05,
  Fun = fun({X, Y}, Pher) -> 
    (1 - Ro) * Pher
    end,
  maps:map(Fun, Pheromones).

better_path(PathA, PathB) ->
	{NodesA, DistA, PathsA} = PathA,
	{NodesB, DistB, PathsB} = PathB,
	if 	DistA =< DistB -> PathA;
		true -> PathB
	end.
	
get_path(Distances, Pheromones, NodesToVisit) ->
	{RevNodes, Dist, RevPaths} = get_rev_path(Distances, Pheromones, NodesToVisit, {[], 0, []}),
  % io:format("Reversed path and distance: ~w, ~w~n", [RevNodes, Dist]),
  {lists:reverse(RevNodes), Dist, lists:reverse(RevPaths)}.
get_rev_path(Distances, Pheromones, NodesToVisit, {[], 0, []}) ->
  % io:format("Nodes to visit before first: ~w~n", [NodesToVisit]),
  {Visited, NewNodesToVisit} = get_first_node(NodesToVisit),
  % io:format("First visited and new nodes to visit: ~w, ~w~n", [Visited, NewNodesToVisit]),
  get_rev_path(Distances, Pheromones, NewNodesToVisit, Visited, {[Visited], 0, []}).
get_rev_path(Distances, Pheromones, [], FirstNode, Path) -> 
  % tutaj chyba trzeba dodac jeszcze ostatni move z ostatniego do pierwszego i dodac distance
  {[Last|RevNodes], Dist, RevPaths} = Path,
  {RevNodes, Dist + maps:get({Last, FirstNode}, Distances), [{Last, FirstNode}|RevPaths]};
get_rev_path(Distances, Pheromones, NodesToVisit, FirstNode, Path) ->
  % io:format("Path before getting next node: ~w~n", [Path]),
  {[Last|Rest], CurrentDistance, CurrentMoves} = Path,
  % io:format("Nodes to visit before getting next node in this path: ~w~n", [NodesToVisit]),
  {Visited, NewNodesToVisit} = get_next_node(Distances, Pheromones, NodesToVisit, Last),
  % io:format("Chosen visited node and nodes to visit after choosing: ~w, ~w~n", [Visited, NewNodesToVisit]),
  get_rev_path(Distances, Pheromones, NewNodesToVisit, FirstNode, {[Visited, Last|Rest], CurrentDistance + maps:get({Last, Visited}, Distances), [{Last, Visited}|CurrentMoves]}).

get_first_node([First|Nodes]) -> {First, Nodes}.

get_next_node(Distances, Pheromones, NodesToVisit, NodeFrom) ->
  % policzyc prawdopodo
  % zamienic je na te sumy
  % wylowowac liczbe i przejsc po liscie tupli (suma, node) wybierajac jedna
  % potem zwrocic tuple z nodem i lista bez niego
  IsPossibleEdge = fun(Key, Value) ->
      {X, Y} = Key, 
      (X =:= NodeFrom) and
      lists:member(Y, NodesToVisit) 
    end,
  %io:format("Beginning node, nodes to choose from: ~w, ~w~n", [NodeFrom, NodesToVisit]),
  PossiblePheromones = maps:filter(IsPossibleEdge, Pheromones),
  %io:format("Possible pheromones: ~w~n", [PossiblePheromones]),
  SumProb = get_sum_probs(Distances, PossiblePheromones),
  %io:format("Sum of probabilities: ~w~n", [SumProb]),
  EdgeProbs = maps:map(fun(Edge, Pheromone) -> get_prob(Edge, maps:get(Edge, Distances), Pheromone) / SumProb end, PossiblePheromones),
  %io:format("Probabilities of choosing possible edges: ~w~n", [EdgeProbs]),
  ListProbs = [{Y, Prob} || {{X, Y}, Prob} <- maps:to_list(EdgeProbs)],
  %io:format("Probabilities of choosing next node: ~w~n", [ListProbs]),
  RandProb = rand:uniform(),
  %io:format("Random number: ~w~n", [RandProb]),
  CumulProbs = get_cumulative_prob_sum(ListProbs),
  %io:format("Cumulative probabilities of choosing next node: ~w~n", [CumulProbs]),
  [FirstProb|RestProbs] = CumulProbs,
  ChooseProb = fun({Node, NodeProb}, {AccNode, AccProb}) -> 
    if  RandProb >= NodeProb -> {Node, NodeProb};
        true -> {AccNode, AccProb}
      end
    end,
  {ChosenNode, Prob} = lists:foldl(ChooseProb, FirstProb, RestProbs),
  NewNodesToVisit = [X || X <- NodesToVisit, X =/= ChosenNode], 
  %io:format("Chosen node and prob: ~w, ~w~n", [ChosenNode, Prob]),
  {ChosenNode, NewNodesToVisit}.

get_cumulative_prob_sum(NodeProbs) -> get_cumulative_prob_sum(NodeProbs, []).
get_cumulative_prob_sum([{First, FirstProb}|NodeProbs], []) -> get_cumulative_prob_sum(NodeProbs, [{First, 0}], FirstProb).
get_cumulative_prob_sum([], AccList, NextProb) -> lists:reverse(AccList);
get_cumulative_prob_sum([Next|NodeProbs], AccList, NextProb) ->
  {Node, NodeProb} = Next,
  get_cumulative_prob_sum(NodeProbs, [{Node, NextProb}|AccList], NextProb + NodeProb).

get_sum_probs(Distances, PossiblePheromones) -> 
  AddProb = fun(Edge, Pheromone, Acc) -> 
    Acc + get_prob(Edge, maps:get(Edge, Distances), Pheromone) end,
  maps:fold(AddProb, 0, PossiblePheromones).

get_prob(Edge, Distance, Pheromone) ->
  Alfa = 1,
  Beta = 1,
  math:pow(Pheromone, Alfa) * math:pow(1 / Distance, Beta).

main(Filename) ->
  {Dimension, Map, Distance} = tsplibReader:init(Filename),
  Iterations = 100,
  NumberOfAnts = 50,
  {Pheromones, Distances} = main_loop(Iterations, NumberOfAnts, Dimension, Distance),
  io:format("Pheromones: ~w~n", [Pheromones]).



