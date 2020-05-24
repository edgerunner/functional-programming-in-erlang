-module(shapes).

-export([enclose/1, perimeter/1]).

% triangle is a right triangle all the time.

perimeter({rect, W, H}) -> (W + H) * 2;
perimeter({circle, R}) -> 2 * R * math:pi();
perimeter({triangle, W, H}) ->
    W + H + math:sqrt(W * W + H * H).

enclose(Rect = {rect, _W, _H}) -> Rect;
enclose({triangle, W, H}) -> {rect, W, H};
enclose({circle, R}) -> {rect, R * 2, R * 2}.
