-module(list).

-export([double/1, evens/1, max/1, nub/1, palindrome/1,
	 product/1, take/2, test/0]).

% when you have a linked list that needs to be folded/reduced to
% the same type as its contents with a commutative operation,
% you don't need a helper function to do tail recursion.
% The head of the list serves as an accumulator.

product([N]) -> N;
product([N1, N2 | Ns]) -> product([N1 * N2 | Ns]).

max([N]) -> N;
max([N1, N2 | Ns]) when N1 < N2 -> max([N2 | Ns]);
max([N1, _N2 | Ns]) -> max([N1 | Ns]).

% list comprehensions transform lists in a concise manner.

double(Ns) -> [2 * N || N <- Ns].

evens(Ns) -> [N || N <- Ns, N rem 2 == 0].

take(0, _) -> [];
take(_, []) -> [];
take(N, [X | Xs]) -> [X | take(N - 1, Xs)].

nub(L) -> nub(L, []).

nub([], Nubs) -> lists:reverse(Nubs);
nub([X | Xs], Nubs) ->
    case lists:member(X, Nubs) of
      true -> nub(Xs, Nubs);
      false -> nub(Xs, [X | Nubs])
    end.

palindrome(Str) ->
    Lower = string:lowercase(Str),
    Clean = lists:filter(fun (Char)
				 when Char >= $a, Char =< $z ->
				 true;
			     (_) -> false
			 end,
			 Lower),
    Reverse = lists:reverse(Clean),
    Zipped = lists:zipwith(fun erlang:'=='/2, Reverse,
			   Clean),
    lists:foldl(fun erlang:'and'/2, true, Zipped).

% TESTS

test_product() ->
    6 = product([1, 2, 3]), 28 = product([2, 7, 2]), ok.

test_max() ->
    3 = max([1, 2, 3]), 7 = max([2, 7, 2]), ok.

test_double() ->
    [2, 4, 6] = double([1, 2, 3]),
    [4, 14, 4] = double([2, 7, 2]),
    ok.

test_evens() ->
    [2] = evens([1, 2, 3]), [2, 8] = evens([2, 7, 8]), ok.

test_take() ->
    [] = take(0, "hello"),
    "hell" = take(4, "hello"),
    "hello" = take(5, "hello"),
    "hello" = take(9, "hello"),
    ok.

test_nub() ->
    [] = nub([]),
    [3, 2, 7] = nub([3, 2, 7]),
    [5] = nub([5, 5, 5]),
    [2, 4, 1, 3] = nub([2, 4, 1, 3, 3, 1]),
    ok.

test_palindrome() ->
    true = palindrome([]),
    true = palindrome("madam"),
    true = palindrome("Madam I'm Adam"),
    false = palindrome("adam"),
    false = palindrome("Madam I'm mad"),
    ok.

test() ->
    ok = test_product(),
    ok = test_max(),
    ok = test_double(),
    ok = test_evens(),
    ok = test_take(),
    ok = test_nub(),
    ok = test_palindrome(),
    ok.
