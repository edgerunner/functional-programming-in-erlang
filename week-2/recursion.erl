-module(recursion).

-export([factorial/1, fibonacci/1, hyper_pieces/2,
	 perfect/1, pieces/1, test/0]).

factorial(N) when N > 0 -> factorial(N, 1).

factorial(1, P) -> P;
factorial(N, P) -> factorial(N - 1, P * N).

fibonacci(N) -> fibonacci(N, 0, 1).

fibonacci(0, _A, B) -> B;
fibonacci(N, A, B) when N > 0 ->
    fibonacci(N - 1, B, A + B).

pieces(N) -> hyper_pieces(2, N).

%% D: number of dimensions, N: number of cuts
hyper_pieces(D, N) when D > 0, N >= 0 ->
    hyper_pieces(N, 0, lists:duplicate(D + 1, 1)).

hyper_pieces(N, N, List) -> lists:last(List);
hyper_pieces(N, C, West) ->
    Northwest = lists:droplast([0 | West]),
    East = lists:zipwith(fun erlang:'+'/2, West, Northwest),
    hyper_pieces(N, C + 1, East).

perfect(Number) -> perfect(Number, 2, 1).

perfect(Number, _, Number) -> true;
perfect(Number, Divisor, Sum)
    when Sum > Number; Divisor > Number ->
    false;
perfect(Number, Divisor, Sum)
    when 0 == Number rem Divisor ->
    perfect(Number, Divisor + 1,
	    Sum + Divisor + Number div Divisor);
perfect(Number, Divisor, Sum) ->
    perfect(Number, Divisor + 1, Sum).

test_fibonacci() ->
    {'EXIT', {function_clause, _}} = (catch fibonacci(-3)),
    1 = fibonacci(1),
    5 = fibonacci(4),
    13 = fibonacci(6),
    55 = fibonacci(9),
    573147844013817084101 = fibonacci(100),
    ok.

test_factorial() ->
    1 = factorial(1),
    6 = factorial(3),
    120 = factorial(5),
    5040 = factorial(7),
    ok.

test_pieces() ->
    {'EXIT', {function_clause, _}} = (catch pieces(-3)),
    1 = pieces(0),
    2 = pieces(1),
    4 = pieces(2),
    7 = pieces(3),
    11 = pieces(4),
    16 = pieces(5),
    500501 = pieces(1000),
    ok.

test_hyper_pieces() ->
    5 = hyper_pieces(1, 4),
    11 = hyper_pieces(2, 4),
    15 = hyper_pieces(3, 4),
    16 = hyper_pieces(4, 4),
    166667501 = hyper_pieces(3, 1000),
    ok.

test_perfect() ->
    true = perfect(6),
    true = perfect(28),
    false = perfect(10),
    false = perfect(25),
    ok.

test() ->
    ok = test_factorial(),
    ok = test_fibonacci(),
    ok = test_pieces(),
    ok = test_hyper_pieces(),
    ok = test_perfect(),
    ok.
