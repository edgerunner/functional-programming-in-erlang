-module(text).

-export([block/3, document_structure/1]).

-spec block(Method :: left | center | right | justify,
	    Width :: pos_integer(), Text :: string()) -> string().

block(Method, Width, Text) ->
    Structure = document_structure(Text),
    Paragraphs = lists:map(fun (Paragraph) ->
				   Lines = wrap_lines(Width, Paragraph),
				   align_lines(Method, Width, Lines)
			   end,
			   Structure),
    string:join(Paragraphs, "\n\n").

%% Regex for a line with nothing but space.
-define(BLANK_LINE, "\\s*\\R\\s*(?:\\R\\s*)+").

%% Regex for runs of whitespace.
-define(WHITESPACE, "\\s+").

-spec document_structure(string()) -> [[string()]].

%% builds a nested list of strings from text.
%% Text -> Paragraphs -> Words
document_structure(Text) ->
    Paragraphs = re:split(Text, ?BLANK_LINE,
			  [{return, list}]),
    lists:map(fun (Paragraph) ->
		      re:split(Paragraph, ?WHITESPACE, [{return, list}, trim])
	      end,
	      Paragraphs).

-spec wrap_lines(Width :: pos_integer(),
		 Paragraphs :: [[string()]]) -> [[[string()]]].

wrap_lines(Width, Words) ->
    wrap_lines(Width, Words, 0, [[]]).

%% If source is empty, we are done. Reverse and dump the destination.
wrap_lines(_Width, [], _Run, [Line | Lines]) ->
    lists:reverse([lists:reverse(Line) | Lines]);
%% Start a new line when the run length exceeds width.
%% Also, reset the run length and reverse the recently finished line
wrap_lines(Width, [Word | Words], Run, [Line | Lines])
    when Width < Run + length(Word) ->
    wrap_lines(Width, Words, length(Word) + 1,
	       [[Word], lists:reverse(Line) | Lines]);
%% Add word to current line otherwise
wrap_lines(Width, [Word | Words], Run,
	   [Line | Lines]) ->
    wrap_lines(Width, Words, Run + length(Word) + 1,
	       [[Word | Line] | Lines]).

align_lines(Method, Width, Lines) ->
    Aligned = lists:map(fun (Line) ->
				align_line(Method, Width, Line)
			end,
			Lines),
    string:join(Aligned, "\n").

align_line(left, _Width, Words) ->
    string:join(Words, " ");
align_line(right, Width, Words) ->
    string:right(string:join(Words, " "), Width);
align_line(center, Width, Words) ->
    string:centre(string:join(Words, " "), Width);
align_line(justify, _Width, [Word]) -> Word;
align_line(justify, Width, Words) ->
    Count = length(Words) - 1,
    Available = Width - line_length(Words),
    Base = Available div Count,
    Step = math:fmod(Available / Count, 1),
    Line = justify_line(Base, Step, 9 / 10, Words),
    string:join(Line, "").

line_length(Words) ->
    lists:sum(lists:map(fun erlang:length/1, Words)).

justify_line(_Base, _Step, _Error, [Word]) -> [Word];
justify_line(Base, Step, Error, [Word | Words])
    when Error >= 1 ->
    [Word, string:chars($\s, Base + 1) | justify_line(Base,
						      Step, Error + Step - 1,
						      Words)];
justify_line(Base, Step, Error, [Word | Words]) ->
    [Word, string:chars($\s, Base) | justify_line(Base,
						  Step, Error + Step, Words)].
