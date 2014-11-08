-module(forwardChaining).
-export([printRules/1]).
-export([execute/2]).
-export([executeNodes/3]).
-export([controller/4]).
-include("rulesParser.hrl").

%% ----------------------------------------------------------------
printRules(RulesFile) ->
	io:format("reglas: ~n"),
	
	PrintRule = fun(Rule) -> 
		printRule(Rule),
		io:format("~n")
	end,
		
	Rules = parseRules(RulesFile),
	lists:foreach(PrintRule, Rules).

%% ----------------------------------------------------------------	
execute(Facts, RulesFile) ->
	T1 = now(),
	io:format("~nhechos iniciales: "),
	printList(Facts),
	
	Rules = parseRules(RulesFile),
	NewFacts = executeRules(Facts, Rules),
	
	io:format("~nhechos resultado: "),
	printList(NewFacts),
	T2 = now(),
	io:format("Tiempo de proceso pid ~w:", [self()]),
	printElapsedTime(T1, T2),

	controller ! {processFinished, self(), T1, T2, NewFacts}.

executeRules(Facts, Rules) ->
	TrueRule = executeNextRule(Rules, Facts),
	
	if
		TrueRule /= noMoreRules ->
			NewFacts = getNewFacts(TrueRule, Facts),
			NewRules = Rules--[TrueRule],
			
			ResultFacts = executeRules(NewFacts, NewRules),
			ResultFacts;
		true ->
			Facts
	end.

%% ----------------------------------------------------------------
getNewFacts({[], Consequent}, Facts) ->
	InArray = isInArray(Facts, Consequent),
	
	if
		InArray ->
			Facts;
		true ->
			NewFacts = Facts++[Consequent],
			NewFacts
	end;
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
executeNextRule([], _) ->
	noMoreRules;
executeNextRule([Rule | RestRules], Facts) ->
	RuleState = getRuleState(Rule, Facts),
	if
		RuleState == isTrue ->
			%%printRuleResult(Rule, Facts, "se cumple:"),
			Rule;
			
		RuleState == isPartialTrue ->
			%%printRuleResult(Rule, Facts, "se cumple parcialmente:"),
			IsTrue = verifyPartialRule(Rule, Facts),
			
			if
				IsTrue == isTrue ->
					%%printRuleResult(Rule, Facts, "se cumple:"),
					Rule;
				true ->
					ResultRule = executeNextRule(RestRules, Facts),
					ResultRule
				end;
				
		true ->
			%%io:format("NO se cumple: "),
			%%printRule(Rule),
			%%io:format("~n"),
			ResultRule = executeNextRule(RestRules, Facts),
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
			io:format("se cumple el elemento ~s", [Antecedent]),
			{ok, [IsTrue]} = io:fread("? [y or n] ", "~s"),
			
			if
				IsTrue == "y" ->
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
	printRule(Rule),
	io:format("~n").
	%%io:format(" con los hechos: "),
	%%printList(Facts).
	
%% ----------------------------------------------------------------
printList([]) ->
	io:format("~n~n");
printList([Element | RestElements]) ->
	io:format("~s,", [Element]),
	printList(RestElements).

%% ----------------------------------------------------------------
printRule({[], Consequent}) ->
	io:format("=> ~s", [Consequent]);
printRule({[Antecedent | RestAntecedents], Consequent}) ->
	io:format("~s", [Antecedent]),
	AntecedentsCount = lists:flatlength(RestAntecedents),
	if
		AntecedentsCount > 0 ->
			io:format(" & ");
		true ->
			io:format(" ")
	end,
	printRule({RestAntecedents, Consequent}).

%% ----------------------------------------------------------------
printElapsedTime(T1, T2) ->
	SecondsStart = element(2,T1) + (element(3,T1)/1000000),
	SecondsEnd = element(2,T2) + (element(3,T2)/1000000),
	Seconds = SecondsEnd - SecondsStart,
	io:format("~7.3f seconds~n", [Seconds]).

%% ---------------- CONTROLLER NODE -------------------------------
executeNodes(Facts, RulesFile, NodesCount) ->
	startControllerNode(NodesCount),
	startNode(NodesCount, 0, Facts, RulesFile).

%% ----------------------------------------------------------------
startNode(NodesCount, NodeNumber, Facts, RulesFile) ->
	if
		NodeNumber < NodesCount ->
			PID = spawn(forwardChaining, execute, [Facts, RulesFile]),
			NewNodeNumber = NodeNumber + 1,
			%%io:format("inicializando nodo ~w pid: ~w~n", [NewNodeNumber, PID]),
			startNode(NodesCount, NewNodeNumber, Facts, RulesFile);

		true ->
			%%io:format("nodos inicializados~n")
			initializedNodes %% nothing to do
	end.

%% ----------------------------------------------------------------
startControllerNode(NodesCount) ->
	TBegin = now(),
	RegNames = registered(),
	IsRegistered = lists:member(controller, RegNames),

	if
	  IsRegistered == false ->
		%%io:format("controlador no registrado~n");
		IsRegistered; %% nothing to do

	  true ->
		exit(whereis(controller), kill),
		unregister(controller)
	end,

	PID = spawn(forwardChaining, controller, [NodesCount, 0, [], TBegin]),
	%%io:format("controlador inicializado pid: ~w~n", [PID]),
	register(controller, PID).

%% ----------------------------------------------------------------
controller(NodesCount, NodesFinishedCount, NodeDataArray, TBegin) ->
	receive
		{processFinished, PID, T1, T2, Facts} ->
			NewNodesFinishedCount = NodesFinishedCount + 1,
			NewNodeDataArray = NodeDataArray++[{PID, T1, T2, Facts}],

			if
				NodesCount == NewNodesFinishedCount ->
					printResult(TBegin, NewNodeDataArray);

				true ->
					controller(NodesCount, NewNodesFinishedCount, NewNodeDataArray, TBegin)
			end
	end.

%% ----------------------------------------------------------------
printResult(TBegin, NodeDataArray) ->
	PrintNodeResult = fun({NodePID, NodeT1, NodeT2, NodeFacts}) -> 
		io:format("hechos resultado: "),
		printList(NodeFacts),
		io:format("Tiempo de proceso pid ~w: ", [NodePID]),
		printElapsedTime(NodeT1, NodeT2)
	end,

	io:format("Proceso total: "),
	printElapsedTime(TBegin, now()),
	io:format("~n"),
	io:format("resultados:~n"),
	lists:foreach(PrintNodeResult, NodeDataArray).


