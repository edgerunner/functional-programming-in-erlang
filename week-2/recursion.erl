-module(recursion).

-export([fibonacci/1, pieces/1, test/0]).

fibonacci(0) -> 1;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 ->
    fibonacci(N - 1) + fibonacci(N - 2).

pieces(0) -> 1;
pieces(1) -> 2;
pieces(N) when N > 1 -> N + pieces(N - 1).

test_fibonacci() ->
    {'EXIT', {function_clause, _}} = (catch fibonacci(-3)),
    1 = fibonacci(1),
    5 = fibonacci(4),
    13 = fibonacci(6),
    ok.

test_pieces() ->
    {'EXIT', {function_clause, _}} = (catch pieces(-3)),
    1 = pieces(0),
    2 = pieces(1),
    4 = pieces(2),
    7 = pieces(3),
    11 = pieces(4),
    16 = pieces(5),
    ok.

test() -> ok = test_fibonacci(), ok = test_pieces(), ok.
