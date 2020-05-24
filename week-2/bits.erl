-module(bits).

-export([bits/1]).

bits(N) -> bits(N, 0).

bits(0, S) -> S;
bits(N, S) when N band 1 == 0 -> bits(N bsr 1, S);
bits(N, S) -> bits(N bsr 1, S + 1).
