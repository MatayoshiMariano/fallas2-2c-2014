-module(nodo).
-export([iniciar/0,evaluar/0,imprimirReglas/0]).

%% ----------------------------------------------------------------	

iniciar() -> register(evaluador, spawn(nodo, evaluar, [])).

%% ----------------------------------------------------------------	

evaluar() ->
	receive
		{admin, Admin_PID,Hechos} -> 
			Reglas = obtenerReglas(),
   			HechosResultantes = ejecutarReglas(Hechos, Reglas),
			Admin_PID ! {nodo,HechosResultantes},
			evaluar()	
	end.

%% ----------------------------------------------------------------
ejecutarReglas(Hechos, Reglas) ->
	TrueRule = ejecutarSiguienteRegla(Reglas, Hechos),
	
	if
		TrueRule /= noMoreRules ->
			NewFacts = getNewFacts(TrueRule, Hechos),
			NewRules = Reglas--[TrueRule],
			
			ResultFacts = ejecutarReglas(NewFacts, NewRules),
			ResultFacts;

		true -> Hechos
	end.


%% ----------------------------------------------------------------
getNewFacts({[], Consequent}, Facts) ->
	NewFacts = Facts++[Consequent],
	NewFacts;
getNewFacts({[Antecedent | RestAntecedents], Consequent}, Facts) ->
	InArray = isInArray(Facts, Antecedent),
	
	if
		InArray ->
			ResultFacts = getNewFacts({RestAntecedents, Consequent}, Facts),
			ResultFacts;
		true ->
			NewFacts = Facts++[Antecedent],
			ResultFacts = getNewFacts({RestAntecedents, Consequent}, NewFacts),
			ResultFacts
	end.
%% ----------------------------------------------------------------
ejecutarSiguienteRegla([], _) ->
	noMoreRules;
ejecutarSiguienteRegla([Rule | RestRules], Facts) ->
	RuleState = getRuleState(Rule, Facts),
	if
		RuleState == isTrue ->
			printRuleResult(Rule, Facts, "Se cumple:"),
			Rule;
			
		RuleState == isPartialTrue ->
			printRuleResult(Rule, Facts, "Se cumple parcialmente:"),
			IsTrue = verifyPartialRule(Rule, Facts),
			
			if
				IsTrue == isTrue ->
					printRuleResult(Rule, Facts, "Se cumple:"),
					Rule;
				true ->
					ResultRule = ejecutarSiguienteRegla(RestRules, Facts),
					ResultRule
				end;
				
		true ->
			io:format("NO se cumple: "),
			imprimirRegla(Rule),
			io:format("~n"),
			ResultRule = ejecutarSiguienteRegla(RestRules, Facts),
			ResultRule
	end.
	
%% ----------------------------------------------------------------
verifyPartialRule({[], _}, _) ->
	isTrue;
verifyPartialRule({[Antecedent | RestAntecedents], Consequent}, Facts) ->
	InArray = isInArray(Facts, Antecedent),
	if
		InArray ->
			verifyPartialRule({RestAntecedents, Consequent}, Facts);
		true ->
			io:format("Se cumple el elemento ~s", [Antecedent]),
			{ok, [IsTrue]} = io:fread("? [s | n] ", "~s"),
			
			if
				IsTrue == "s" ->
					verifyPartialRule({RestAntecedents, Consequent}, Facts);
				true ->
					isFalse
			end
	end.
	
%% ----------------------------------------------------------------
getRuleState(Rule, Facts) ->
	IsTrue = isTrueRule(Rule, Facts),
	IsPartialTrue = isPartialTrueRule(Rule, Facts),
	
	if
		IsTrue == isTrue ->
			isTrue;
		IsPartialTrue == isPartialTrue ->
			isPartialTrue;
		true ->
			isFalse
	end.

%% ----------------------------------------------------------------
isTrueRule({[], _}, _) ->
	isTrue;
isTrueRule({[Antecedent | RestAntecedents], Consequent}, Facts) ->
	InArray = isInArray(Facts, Antecedent),
	if
		InArray ->
			Result = isTrueRule({RestAntecedents, Consequent}, Facts),
			Result;
		true ->
			isFalse
	end.
	
%% ----------------------------------------------------------------
isPartialTrueRule({[], _}, _) ->
	isFalse;
isPartialTrueRule({[Antecedent | RestAntecedents], Consequent}, Facts) ->
	InArray = isInArray(Facts, Antecedent),
	if
		InArray ->
			isPartialTrue;
		true ->
			Result = isPartialTrueRule({RestAntecedents, Consequent}, Facts),
			Result
	end.
	
%% ----------------------------------------------------------------
isInArray([], _) ->
	false;
isInArray([Element | RestElements], SearchedElement) ->
	if
		Element == SearchedElement ->
			true;
		true ->
			isInArray(RestElements, SearchedElement)
	end.

%% ----------------------------------------------------------------
printRuleResult(Rule, Facts, Message) ->
	io:format("~s ", [Message]),
	imprimirRegla(Rule),
	io:format(" Con los hechos: "),
	printList(Facts).
	
%% ----------------------------------------------------------------
printList([]) ->
	io:format("~n~n");
printList([Element | RestElements]) ->
	io:format("~w,", [Element]),
	printList(RestElements).

%% ----------------------------------------------------------------
imprimirReglas() ->
	io:format("Reglas: ~n"),
	
	ImprimirRegla = fun(Regla) -> 
				imprimirRegla(Regla),
				io:format("~n")
			end,
		
	Reglas = obtenerReglas(),
	lists:foreach(ImprimirRegla, Reglas).

%% ----------------------------------------------------------------
imprimirRegla({[], Consequent}) ->
	io:format("=> ~w", [Consequent]);
imprimirRegla({[Antecedent | RestAntecedents], Consequent}) ->
	io:format("~w", [Antecedent]),
	AntecedentsCount = lists:flatlength(RestAntecedents),
	if
		AntecedentsCount > 0 ->
			io:format(" & ");
		true ->
			io:format(" ")
	end,
	imprimirRegla({RestAntecedents, Consequent}).

%% ----------------------------------------------------------------
obtenerReglas() ->
	[{[a], b}, {[b], c}, {[a, e], d},{[f,g,h],m}].
