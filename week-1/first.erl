-module(first).

-export([area/3, double/1, mult/2, square/1, treble/1]).

mult(X, Y) -> X * Y.

double(X) -> mult(2, X).

treble(X) -> mult(3, X).

square(X) -> mult(X, X).

area(A, B, C) ->
    S = (A + B + C) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).
