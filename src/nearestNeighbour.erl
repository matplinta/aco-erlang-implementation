% #!/usr/bin/env escript
-module(nearestNeighbour).
% -import(tsplibReader)
% -compile([solve/1]).
-export([solve/2]).
% -compile([[solve/2]]).

solve(Dimension, Distance) ->
    CurrentCity = rand:uniform(Dimension),
    % io:format("~w", [CurrentCity]),
    CitiesToVisit = [ X || X <- lists:seq(1, Dimension), CurrentCity =/= X],
    % io:format("~w~n", [CitiesToVisit]),
    % Solution = [CurrentCity],
    Sol = nearestCities(Distance, CitiesToVisit, CurrentCity),
    Solution = lists:append(lists:append([CurrentCity], Sol), [CurrentCity]),
    % io:format("~w~n", [Solution]),
    Solution.
    

nearestCities(Dist, CitiesToVisit, City) -> nearestCities(Dist, CitiesToVisit, City, []).
nearestCities(_Dist, [], _City, Solution) -> Solution;
nearestCities(Dist, CitiesToVisit, City, Solution) -> 
    CitiesDistance = lists:keysort(2, [{J, maps:get({City, J}, Dist)} || J <- CitiesToVisit]),
    [{NextCity, _NCDist} | _Rest] = CitiesDistance,
    CitiesToVisitReduced = lists:delete(NextCity, CitiesToVisit),
    nearestCities(Dist, CitiesToVisitReduced, NextCity, lists:append(Solution, [NextCity])).

