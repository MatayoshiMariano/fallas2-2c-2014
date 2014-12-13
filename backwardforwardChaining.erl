-module(backwardforwardChaining).
-export([printRules/1]).
-export([controller/6]).
-export([startRulesNode/3]).
-export([rulesNodeController/1]).
-export([executeRules/2]).
-export([startControllerNode/0]).
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
executeNodeRules(Facts, Rules, TrueRules) ->
  % io:format("Empieza executeNodeRules ~n"),
  TrueRule = executeNextRule(Rules, Facts),
  if
    TrueRule /= noMoreRules ->
      % io:format("TrueRule /= noMoreRules ~n"),
      NewFacts = getNewFacts(TrueRule, Facts),
      NewRules = Rules--[TrueRule],
      NewTrueRules = TrueRules++[TrueRule],

      Result = executeNodeRules(NewFacts, NewRules, NewTrueRules),
      Result;
    true ->
      TrueRules
  end.

%% ----------------------------------------------------------------
executeNextRule([], _) ->
  noMoreRules;

executeNextRule([Rule | RestRules], Facts) ->
  % io:format("~n"),
  % io:format("~n"),
  % io:format("Empieza executeNextRule ~n"),
  RuleState = getRuleState(Rule, Facts),
  if
    RuleState == isTrue ->
      printRuleResult(Rule, Facts, "se cumple:"),
      Rule;

    RuleState == isPartialTrue ->
      printRuleResult(Rule, Facts, "se cumple parcialmente:"),
      IsTrue = verifyPartialRule(Rule, Facts),

      if
        IsTrue == isTrue ->
          printRuleResult(Rule, Facts, "se cumple:"),
          Rule;
        true ->
          ResultRule = executeNextRule(RestRules, Facts),
          ResultRule
        end;

    true ->
      io:format("NO se cumple: "),
      printRule(Rule),
      io:format("~n"),
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
  % io:format("Empieza getRuleState ~n"),
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
  % io:format("Empieza isTrueRule sin parametros ~n"),
  isTrue;
isTrueRule({[Antecedent | RestAntecedents], Consequent}, Facts) ->
  % io:format("Empieza isTrueRule ~n"),
  % io:format("Antecedent:"),
  % io:format(Antecedent),
  % io:format("~n"),
  % io:format("RestAntecedents:"),
  % io:format(RestAntecedents),
  % io:format("~n"),
  % io:format("Consequent:"),
  % io:format(Consequent),
  % io:format("~n"),
  BackwardInArray = isInArray(Facts, Consequent),
  ForwardInArray = isInArray(Facts, Antecedent),
  if
    ForwardInArray ->
      Result = isTrueRule({RestAntecedents, Consequent}, Facts),
      Result;
    BackwardInArray ->
      Result = isTrueRule({RestAntecedents, Consequent}, Facts),
      Result;
    true ->
      isFalse
  end.

%% ----------------------------------------------------------------
isPartialTrueRule({[], _}, _) ->
  % io:format("Empieza isPartialTrueRule sin parametros ~n"),
  isFalse;
isPartialTrueRule({[Antecedent | RestAntecedents], Consequent}, Facts) ->
  % io:format("Empieza isPartialTrueRule ~n"),
  % io:format("Antecedent:"),
  % io:format(Antecedent),
  % io:format("~n"),
  % io:format("RestAntecedents:"),
  % io:format(RestAntecedents),
  % io:format("~n"),
  % io:format("Consequent:"),
  % io:format(Consequent),
  % io:format("~n"),
  BackwardInArray = isInArray(Facts, Consequent),
  ForwardInArray = isInArray(Facts, Antecedent),
  if
    ForwardInArray ->
      isPartialTrue;
    BackwardInArray ->
      isPartialTrue;
    true ->
      Result = isPartialTrueRule({RestAntecedents, Consequent}, Facts),
      Result
  end.

%% ----------------------------------------------------------------
isInArray([], _) ->
  % io:format("Empieza isInArray sin parametros ~n"),
  false;
isInArray([Element | RestElements], SearchedElement) ->
  % io:format("Empieza isInArray ~n"),
  % io:format("Element:"),
  % io:format(Element),
  % io:format("~n"),
  % io:format("RestElements:"),
  % io:format(RestElements),
  % io:format("~n"),
  % io:format("SearchedElement:"),
  % io:format(SearchedElement),
  % io:format("~n"),
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
  io:format("~n"),
  io:format(" con los hechos: "),
  printList(Facts).

%% ----------------------------------------------------------------
printList([]) ->
  io:format("~n~n");
printList([Element | RestElements]) ->
  io:format("~s,~n", [Element]),
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
  io:format("~7.3f segundos~n", [Seconds]).

%% ---------------- RULES NODE -------------------------------
startRulesNode(RegisterName, CurrentNode, ControllerNode) ->
  startNode(RegisterName, rulesNodeController, [ControllerNode]),

  {controller, ControllerNode} ! {addNode, RegisterName, CurrentNode},
  io:format("registrando nodo en ~w~n", [ControllerNode]).

%% ----------------------------------------------------------------
rulesNodeController(ControllerNode) ->
  receive
    %  Rules tiene las relaciones extraidas del txt
    %  Facts tiene los matches
    {executeRules, Rules, Facts, NodeNumber} ->
      io:format("en executeRules~n"),
      % io:format(Rules),
      printList(Facts),
      io:format("~n"),
      TrueRules = executeNodeRules(Facts, Rules, []),

      {controller, ControllerNode} ! {executionResult, TrueRules, NodeNumber},

      rulesNodeController(ControllerNode);

    stopNode ->
      io:format("parando nodo~n")
  end.

%% ---------------- CONTROLLER NODE -------------------------------
executeRules(Facts, RulesFile) ->
  io:format("~nhechos iniciales: "),
  printList(Facts),

  Rules = parseRules(RulesFile),

  controller ! {executeRules, Rules, Facts},
  io:format("procesando...~n").

%% ----------------------------------------------------------------
startControllerNode() ->
  startNode(controller, controller, [0, 0, 0, [], [], 0]).

%% ----------------------------------------------------------------
controller(NodesWithChanges, NodesWithoutChanges, TBegin, Facts, NodesDataArray, RulesPerNode) ->
  receive
    {addNode, RegisterName, NodeId} ->
      NewNodesDataArray = NodesDataArray++[{length(NodesDataArray), RegisterName, NodeId, []}],
      io:format("registrando nodo ~w~n", [NodeId]),
      controller(NodesWithChanges, NodesWithoutChanges, TBegin, Facts, NewNodesDataArray, RulesPerNode);

    {executeRules, Rules, NewFacts} ->
      if
        length(NodesDataArray) > 0 ->
          NewTBegin = now(),
          NewRulesPerNode = length(Rules) div length(NodesDataArray),
          io:format("reglas por nodo: ~w~n", [NewRulesPerNode]),
          NewNodesDataArray = executeRulesInNodes(Rules, NewFacts, NewRulesPerNode, NodesDataArray, 0),

          controller(0, 0, NewTBegin, NewFacts, NewNodesDataArray, NewRulesPerNode);

        true ->
          io:format("no hay nodos registrados~n"),

          controller(0, 0, 0, [], NodesDataArray, 0)
      end;

    {executionResult, TrueRules, NodeNumber} ->
      if
        length(TrueRules) == 0 ->
          NewNodesWithoutChanges = NodesWithoutChanges + 1,
          NewNodesWithChanges = NodesWithChanges,
          NewNodesDataArray = NodesDataArray,
          AuxFacts = Facts;

        true ->
          NewNodesWithoutChanges = NodesWithoutChanges,
          NewNodesWithChanges = NodesWithChanges + 1,

          AuxFacts = addRulesToFacts(TrueRules, Facts),
          {_, _, _, NodeRules} = lists:keyfind(NodeNumber, 1, NodesDataArray),

          NewRules = NodeRules--TrueRules,
          NewNodesDataArray = registerRulesForNode(NodesDataArray, NodeNumber, NewRules)
      end,

      % io:format("registrando resultado del nodo ~w~n", [NodeNumber]),
      % io:format("registrando resultado ~w - ~w~n",[NewNodesWithoutChanges, NewNodesWithChanges]),

      NodesCount = length(NodesDataArray),
      % io:format("~nAntes del if: ~n"),
      if
        NodesCount == NewNodesWithoutChanges ->
        %% terminar
          io:format("~nhechos resultado: ~n"),
          printList(AuxFacts),
          io:format("~nProceso total: "),
          printElapsedTime(TBegin, now()),

          controller(0, 0, 0, [], NewNodesDataArray, 0);

        NodesCount == (NewNodesWithChanges + NewNodesWithoutChanges) ->
        %% reiniciar nodos
          restartRulesNodes(NewNodesDataArray, AuxFacts),
          controller(0, 0, TBegin, AuxFacts, NewNodesDataArray, RulesPerNode);

        true -> %% seguir esperando resultados
          controller(NewNodesWithChanges, NewNodesWithoutChanges, TBegin, AuxFacts, NewNodesDataArray, RulesPerNode)
      end;

    stopNode ->
      io:format("parando controller~n")
  end.

%% ----------------------------------------------------------------
addRulesToFacts([], Facts) ->
  Facts;
addRulesToFacts([TrueRule | RestTrueRules], Facts) ->
  NewFacts = getNewFacts(TrueRule, Facts),
  ResultFacts = addRulesToFacts(RestTrueRules, NewFacts),
  ResultFacts.

%% ----------------------------------------------------------------
registerRulesForNode(NodesDataArray, NodeNumber, Rules) ->
  {_, RegisterName, NodeId, _} = lists:keyfind(NodeNumber, 1, NodesDataArray),
  NewNodesDataArray = lists:keyreplace(NodeNumber, 1, NodesDataArray, {NodeNumber, RegisterName, NodeId, Rules}),
  NewNodesDataArray.

%% ----------------------------------------------------------------
executeRulesInNodes([], _, _, NodesDataArray, _) ->
  NodesDataArray;

executeRulesInNodes(Rules, Facts, RulesPerNode, NodesDataArray, CurrentNodeNumber) ->
  RulesCount = length(Rules),

  if
    %% esta condicion es para agregar todas las reglas finales al ultimo nodo
    RulesCount < (RulesPerNode * 2) ->
    io:format("ultimo nodo con ~w reglas~n", [RulesCount]),
    NodeRules = Rules,
    RestRules = [];

    true ->
    {NodeRules, RestRules} = lists:split(RulesPerNode, Rules)
  end,

  {_, RegisterName, NodeId, _} = lists:keyfind(CurrentNodeNumber, 1, NodesDataArray),
  NewNodesDataArray = registerRulesForNode(NodesDataArray, CurrentNodeNumber, NodeRules),

  io:format("iniciando nodo ~w~n", [CurrentNodeNumber]),
  {RegisterName, NodeId} ! {executeRules, NodeRules, Facts, CurrentNodeNumber},

  ResultNodesDataArray = executeRulesInNodes(RestRules, Facts, RulesPerNode, NewNodesDataArray, CurrentNodeNumber + 1),
  ResultNodesDataArray.

%% ----------------------------------------------------------------
restartRulesNodes(NodesDataArray, Facts) ->
  RestartRuleNode = fun({CurrentNodeNumber, RegisterName, NodeId, NodeRules}) ->
    {RegisterName, NodeId} ! {executeRules, NodeRules, Facts, CurrentNodeNumber}
  end,
  lists:foreach(RestartRuleNode, NodesDataArray).

%% -------------------- COMMON NODES FUNCTIONS --------------------------------------------
startNode(RegisterName, FunctionName, ParamsArray) ->
  RegNames = registered(),
  IsRegistered = lists:member(RegisterName, RegNames),

  if
    IsRegistered == false ->
    IsRegistered; %% nothing to do

    true ->
    exit(whereis(RegisterName), kill),
    unregister(RegisterName)
  end,

  PID = spawn(backwardforwardChaining, FunctionName, ParamsArray),
  register(RegisterName, PID).

