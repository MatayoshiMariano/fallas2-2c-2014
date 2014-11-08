-export([parseRules/1]).

%% ----------------------------------------------------------------
parseRules(Filename) ->
     {ok, File} = file:open(Filename, [raw, read, read_ahead]),
     Rules = parseNextRule([],File),
     file:close(File),
     Rules.

%% ----------------------------------------------------------------
parseNextRule(Rules, File) ->
	case file:read_line(File) of
		{ok, RuleLine} ->
			StrIndex = string:str(RuleLine, " => ") - 1,
			StrLastIndex = string:str(RuleLine, "\n") - 1,
			Antecedents = string:tokens(string:sub_string(RuleLine, 1, StrIndex), ","),
			Consequent = string:sub_string(RuleLine, StrIndex + 5, StrLastIndex),
			Rule = {Antecedents, Consequent},

			parseNextRule([Rule|Rules], File);
		eof ->
			lists:reverse(Rules)
	end.
