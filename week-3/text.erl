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

%% Run each line through the alignment function,
%% and then join the results with newlines,
%% returning a complete paragraph.
align_lines(Method, Width, Lines) ->
    Aligned = lists:map(fun (Line) ->
				align_line(Method, Width, Line)
			end,
			Lines),
    string:join(Aligned, "\n").

%% Produce a single string from the list of words
%% that make a pre-determined line. It is assumed
%% that the words fit the given width when joined
%% with a single space.
-spec align_line(Method :: left | right | center |
			   justify,
		 Width :: pos_integer(),
		 Words :: [string()]) -> string().

-define(STARTING_ERROR, 9 / 10).

%% for a left align, just join the list of words
%% with a single space.
align_line(left, _Width, Words) ->
    string:join(Words, " ");
%% pad the joned string right
align_line(right, Width, Words) ->
    string:right(string:join(Words, " "), Width);
%% pad the joined string on both sides
align_line(center, Width, Words) ->
    string:centre(string:join(Words, " "), Width);
%% skip justification on a single-word line
align_line(justify, _Width, [Word]) -> Word;
%% calculate parameters and initiate
%% the justification function
align_line(justify, Width, Words) ->
    % the number of intermediate spaces
    Count = length(Words) - 1,
    % spaces that must be distributed between words
    Available = Width - line_length(Words),
    % the base number of spaces for each space position
    Base = Available div Count,
    % the error accured on every space placed
    Step = math:fmod(Available / Count, 1),
    % call the justification function
    Line = justify_line(Base, Step, ?STARTING_ERROR, Words),
    string:join(Line, "").

%% helper to get the total character count
%% in a list of strings
line_length(Words) ->
    lists:sum(lists:map(fun erlang:length/1, Words)).

%% A recursive error-diffusion function that places
%% the base number of spaces between words until the
%% error meets/exceeds the threshold, which then adds
%% a one-bigger space. This is supposed to evenly
%% distribute the leftover space between the words.
-spec justify_line(Base :: pos_integer(),
		   Step :: float(), Error :: float(),
		   Words :: [string()]) -> [string()].

%% Base case: return the last word as is.
justify_line(_Base, _Step, _Error, [Word]) -> [Word];
%% Error threshold exceeded: add one extra space and reduce error by one
justify_line(Base, Step, Error, [Word | Words])
    when Error >= 1 ->
    [Word, string:chars($\s, Base + 1) | justify_line(Base,
						      Step, Error + Step - 1,
						      Words)];
%% Recursive case: add base space and increment error by step.
justify_line(Base, Step, Error, [Word | Words]) ->
    [Word, string:chars($\s, Base) | justify_line(Base,
						  Step, Error + Step, Words)].
