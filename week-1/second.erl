-module(second).

-export([area/2, hypotenuse/2, perimeter/2]).

-import(first, [square/1]).

hypotenuse(B, H) -> math:sqrt(square(B) + square(H)).

perimeter(B, H) -> hypotenuse(B, H) + B + H.

area(B, H) -> B * H / 2.
