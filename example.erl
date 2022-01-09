-module(example).
% -import(lists, [foreach/2]).
-export([main/1, getNodes/2, getEdgesbyNodeName/3, func/0, duplicateTable/1]).


func() ->
  A = 1,
  if A == 1 ->
    ok;
  true ->
    ok
  end,
  A.


inc(N) -> N + 1. 

main(Ls) ->
  assignEdges(red, table, Ls, 0).


assignEdgesHelper(Name, [], Acc) -> Acc;

                        % lists:nth(2, Graph)
assignEdgesHelper(Name, [H|T], Acc) ->

  Data = tuple_to_list(H),

  Node = lists:nth(1, Data),
  io:format (" Node is ~w~n", [Node]),

  if 
    Node == Name -> 
      assignEdgesHelper(Name, T, Acc+1);
    true -> assignEdgesHelper(Name, T, Acc) 
  end.

assignEdges(Name, Table, [], Res) -> Res;
                        % Graph
assignEdges(Name, Table, [H|T], Res) -> 
  Data = tuple_to_list(H),
  % io:format("~w~n", [Data]),
  Result = assignEdgesHelper(Name, lists:nth(2,Data), Res),
  assignEdges(Name, Table, T, Result).


getNodes([], Nodes) -> Nodes; 
getNodes([H|T], Nodes) -> 
  Data = tuple_to_list(H),
  Node = lists:nth(1,Data),
  getNodes(T, Nodes ++ [Node]).


% assign(Name, Table, Graph) ->
%   Edges = [0],
%   lists:foreach(
%   fun(Item) -> 
%     Data = tuple_to_list(Item),
%     lists:foreach(
%     fun(Element) ->
%       ElemToList = tuple_to_list(Element),
%       Node = lists:nth(1, ElemToList),
%       if 
%         Node == Name -> 
%           Shadow = lists:append(Edges, [1]),
%           Edges = Shadow,
%           io:format ("~w~n", [Node]);
          
%         true -> ok 
%       end
%     end, lists:nth(2, Data))
%   end, Graph),
%   Edges.

getEdgesbyNodeName([], NodeName, Edges) -> Edges;
getEdgesbyNodeName([H|T], NodeName, Edges) ->
  ListItem = tuple_to_list(H),
  Node = lists:nth(1, ListItem),
  if 
    Node == NodeName ->
      E = lists:nth(2, ListItem),
      lists:append(Edges,E);
    true ->
      getEdgesbyNodeName(T, NodeName, Edges)
  end.


duplicateTableHelper('$end_of_table', Table, NewTable) -> NewTable;
duplicateTableHelper(Key, Table, NewTable) ->
    TableEntry = lists:nth(1, ets:lookup(Table, Key)),
    ets:insert(NewTable, TableEntry),
    NextKey = ets:next(Table, Key),
    duplicateTableHelper(NextKey, Table, NewTable).


duplicateTable(Table) ->
    FirstKey = ets:first(Table),
    NewTable = ets:new(lookup_table, [set, public]),
    duplicateTableHelper(FirstKey, Table, NewTable).

simpleNetworkGraph () ->
  [{red  , [{white, [white, green]},
	    {blue , [blue]}]},
   {white, [{red, [blue]},
	    {blue, [green, red]}]},
   {blue , [{green, [white, green, red]}]},
   {green, [{red, [red, blue, white]}]}
  ].

  











