-module(index).

-export([find/2, from_file/1]).

%% This function builds an index from a given file.
from_file(Name) ->
    % Get a handle for the file
    File = get_file(Name),
    % Extract every word, paired with its line number
    Tokens = tokenize_file(File),
    % Drop short tokens
    UsefulTokens = filter_tokens(3, Tokens),
    % Build the searchable data structure
    Index = build(UsefulTokens),
    Index.

%% Gets a file handle for processing
get_file(Name) ->
    {ok, File} = file:open(Name, [read]), File.

%% Initializes tokenize_file/3 with the initial line number
%% and a blank work-in-progress token
tokenize_file(File) ->
    tokenize_file(File, 1, [{wip, ""}]).

%% Reads the file character by character.
%% Builds a list of tokens from the characters
%% in the form {<line_number>, <token_string>}
tokenize_file(File, Line, [{wip, Token} | Tokens]) ->
    ReadChar = io:get_chars(File, "", 1),
    case ReadChar of
      % finalize and output token list at end of file
      eof ->
	  FinalToken = {Line, lists:reverse(Token)},
	  ReverseTokens = [FinalToken | Tokens],
	  lists:reverse(ReverseTokens);
      % finalize token and increment line number on a newline
      "\n" ->
	  DoneToken = {Line, lists:reverse(Token)},
	  tokenize_file(File, Line + 1,
			[{wip, ""}, DoneToken | Tokens]);
      % finalize token on a space
      " " ->
	  DoneToken = {Line, lists:reverse(Token)},
	  tokenize_file(File, Line,
			[{wip, ""}, DoneToken | Tokens]);
      % downcase uppercase character and add to current token
      [Char] when Char >= $A, Char =< $Z ->
	  Lower = Char + 32,
	  UpdatedToken = {wip, [Lower | Token]},
	  tokenize_file(File, Line, [UpdatedToken | Tokens]);
      % add lowercase character to current token
      [Char] when Char >= $a, Char =< $z ->
	  UpdatedToken = {wip, [Char | Token]},
	  tokenize_file(File, Line, [UpdatedToken | Tokens]);
      % add decimal digit to current token
      [Char] when Char >= $0, Char =< $9 ->
	  UpdatedToken = {wip, [Char | Token]},
	  tokenize_file(File, Line, [UpdatedToken | Tokens]);
      % ignore all other characters
      _ -> tokenize_file(File, Line, [{wip, Token} | Tokens])
    end.

%% Removes tokens shorter than the given length
filter_tokens(Length, Tokens) ->
    lists:filter(fun ({_, Token})
			 when length(Token) < Length ->
			 false;
		     (_) -> true
		 end,
		 Tokens).

%% Updates the given index with a new token.
%% A new path is created if needed, and the page
%% number is added to the leaf node. If the path
%% exists, the new page number is added to the leaf
%% node, which is a set, so that no duplicate
%% numbers are stored.
insert_token({Line, ""}, Index) ->
    dict:update(lines,
		fun (Lines) -> ordsets:add_element(Line, Lines) end,
		ordsets:from_list([Line]), Index);
insert_token({Line, [Char | Chars]}, Index) ->
    dict:update(Char,
		fun (InnerIndex) ->
			insert_token({Line, Chars}, InnerIndex)
		end,
		insert_token({Line, Chars}, dict:new()), Index).

%% Builds an index data structure by running insert_token/2
%% on every token in the given list.
build(Tokens) ->
    lists:foldl(fun insert_token/2, dict:new(), Tokens).

%% Traverses the given index data structure with the
%% given string as input, to find a leaf node containing
%% the list of line numbers. Returns an empty list if
%% no match is found.
find("", Index) ->
    % The base case: we are at the end of the query string
    case dict:find(lines, Index) of
      % There should be a key called 'lines' if this query was
      % in the file. It contains the set of line numbers where
      % it exists. We return that as a list if found.
      {ok, Lines} -> ordsets:to_list(Lines);
      % or we return an empty list, meaning that there was no match.
      _ -> []
    end;
find([Char | Chars], Index) ->
    % The recursion case. We check the first character
    case dict:find(Char, Index) of
      % If found, then we keep searching in the branch
      {ok, Subindex} -> find(Chars, Subindex);
      % otherwise we return the empty list, which means there was no match.
      _ -> []
    end.
