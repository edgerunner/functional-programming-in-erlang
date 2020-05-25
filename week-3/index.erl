-module(index).

-export([find/2, from_file/1]).

from_file(Name) ->
    File = get_file(Name),
    Tokens = tokenize_file(File),
    UsefulTokens = filter_tokens(Tokens),
    Index = build(UsefulTokens),
    Index.

get_file(Name) ->
    {ok, File} = file:open(Name, [read]), File.

tokenize_file(File) ->
    tokenize_file(File, 1, [{wip, ""}]).

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

filter_tokens(Tokens) ->
    lists:filter(fun ({_, Token}) when length(Token) < 4 ->
			 false;
		     (_) -> true
		 end,
		 Tokens).

insert_token({Line, ""}, Dict) ->
    dict:update(lines,
		fun (Lines) -> ordsets:add_element(Line, Lines) end,
		ordsets:from_list([Line]), Dict);
insert_token({Line, [Char | Chars]}, Dict) ->
    dict:update(Char,
		fun (InnerDict) ->
			insert_token({Line, Chars}, InnerDict)
		end,
		insert_token({Line, Chars}, dict:new()), Dict).

build(Tokens) ->
    lists:foldl(fun insert_token/2, dict:new(), Tokens).

find("", Index) ->
    case dict:find(lines, Index) of
      {ok, Lines} -> ordsets:to_list(Lines);
      _ -> []
    end;
find([Char | Chars], Index) ->
    case dict:find(Char, Index) of
      {ok, Subindex} -> find(Chars, Subindex);
      _ -> []
    end.
