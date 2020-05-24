-module(list).

-export([max/1, product/1, test/0]).

% when you have a linked list that needs to be folded/reduced to
% the same type as its contents with a commutative operation,
% you don't need a helper function to do tail recursion.
% The head of the list serves as an accumulator.

product([N]) -> N;
product([N1, N2 | Ns]) -> product([N1 * N2 | Ns]).

max([N]) -> N;
max([N1, N2 | Ns]) when N1 < N2 -> max([N2 | Ns]);
max([N1, _N2 | Ns]) -> max([N1 | Ns]).

test_product() ->
    6 = product([1, 2, 3]), 28 = product([2, 7, 2]), ok.

test_max() ->
    3 = max([1, 2, 3]), 7 = max([2, 7, 2]), ok.

test() -> ok = test_product(), ok = test_max(), ok.
