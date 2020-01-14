-module(reader).
-export([init/1]).
% -compile([]).

% VARIABLES FORMAT

% Dimension:    integer
% Map:          NodeNumber => {X, Y}
% Distance:     {NodeNum1, NodeNum2} => distance

strip(S) -> re:replace(S, "\n", "", [global,{return,list}]).
split(S, Sep) -> string:tokens(S, Sep).
to_i(S) -> {I, _} = string:to_integer(S), I.
to_i_list(S) -> lists:map(fun (X) -> to_i(X) end, split(strip(S), " ")).

init(FileName) ->
    Contents = read_lines(FileName),
    Lines = string:split(Contents, "\n", all),
    Dimension = read_dimension(Lines),
    Map = read_nodes(Lines),
    Distance = fill_same(fill_distance_map(Map)),
    {Dimension, Map, Distance}.

read_lines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.

read_dimension([]) -> false;
read_dimension(["DIMENSION:" ++ Dimension | _T]) -> to_i(string:trim(Dimension));
read_dimension([ _H | T ]) -> read_dimension(T).

read_nodes(Line) -> read_nodes(Line, #{}).
read_nodes([], Acc) -> Acc;
read_nodes([ Line | T ], Acc) -> 
    try
        [N, X, Y] = to_i_list(Line),
        case is_integer(N) of
            true ->
                Acc2 = maps:put(N, {X, Y}, Acc),
                read_nodes(T, Acc2);
            false ->
                read_nodes(T, Acc)
        end
    catch error:{badmatch, _V } -> 
        read_nodes(T, Acc)
    end.

euclidean_distance({X1, Y1}, {X2, Y2}) ->
    XDistance = abs(X1 - X2),
    YDistance = abs(Y1 - Y2),
    Distance = math:sqrt(math:pow(XDistance, 2) + math:pow(YDistance, 2)),
    Distance.

fill_distance_map(Map) -> fill_distance_map(Map, #{}).

fill_distance_map(Map, Acc) -> 
    It1 = maps:iterator(Map),
    It2 = maps:iterator(Map),
    loop(Map, It1, It2, Acc).

loop(_Map, none, none, Acc) -> Acc;
loop(Map, Iter_i, none, Acc) -> 
    Iterator = maps:iterator(Map),
    {_K, {_X, _Y}, NewIter_i} = maps:next(Iter_i),
    loop(Map, NewIter_i, Iterator, Acc);
loop(Map, Iter_i, Iter_j, Acc) -> 
    case maps:next(Iter_i) of
        none ->  loop(Map, none, none, Acc);
        {K1, {X1, Y1}, _} ->
            % io:format("I1: ~w, ~w, ~w\n", [K1, X1, Y1]),
            {K2, {X2, Y2}, NewIter_j} = maps:next(Iter_j),
            % io:format(" I2: ~w, ~w, ~w, ~w\n", [K2, X2, Y2, Iter_j]),
            Acc2 = maps:put({K1, K2}, euclidean_distance({X1, Y1}, {X2, Y2}), Acc),
            loop(Map, Iter_i, NewIter_j, Acc2)
    end.

fill_same(Map) -> It = maps:iterator(Map), fill_same(It, Map).
fill_same(none, Acc) -> Acc;
fill_same(It, Acc) -> 
    case maps:next(It) of
            none ->  fill_same(none, Acc);
            {{K1, K2}, D, NewIt} ->
                Acc2 = maps:put({K2, K1}, D, Acc),
                fill_same(NewIt, Acc2)
    end.
