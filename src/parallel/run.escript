#!/usr/bin/env escript

-import(main, [run/0]).
-export([main/1]).

main([String]) ->
    try
        N = list_to_integer(String),
        F = fac(N),
        io:format("factorial ~w = ~w\n", [N,F])
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    usage(),
    main:run().

usage() ->
    io:format("usage: factorial integer\n"),
    halt(1).
fac(0) -> 1;
fac(N) -> N * fac(N-1).

% main(_) -> 
%     main:run().
