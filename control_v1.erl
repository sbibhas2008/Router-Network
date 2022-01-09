-module(control).
-export([graphToNetwork/1, extendNetwork/4]).

assignEdgesHelper(Name, [], Acc) -> Acc;
                        % lists:nth(2, Graph)
assignEdgesHelper(Name, [H|T], Acc) ->

  Data = tuple_to_list(H),

  Node = lists:nth(1, Data),
  % io:format (" Node is ~w~n", [Node]),
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

startNodes([], ControllerTable) -> ok;
startNodes([H|T], ControllerTable) -> 
  RouterPid = router:start(H),
  ets:insert(ControllerTable, {H, RouterPid}),
  startNodes(T, ControllerTable).

getEdgesbyNodeName([], NodeName, Edges) -> Edges;
getEdgesbyNodeName([H|T], NodeName, Edges) ->
  ListItem = tuple_to_list(H),
  Node = lists:nth(1, H),
  if 
    Node == NodeName ->
      Edges = lists:nth(2, H);
    true ->
      getEdgesbyNodeName(T, NodeName, Edges)
  end.

% helper function to unpack the list
insertToTableHelper (_, _, _, []) -> ok;
insertToTableHelper (ControllerTable, RouterTable, Dest, [H|T]) ->
    DestPidLookup = ets:lookup(ControllerTable, Dest),
    Item = lists:nth(1, DestPidLookup),
    ItemLs = tuple_to_list(Item),
    DestPid = lists:nth(2, ItemLs),
    ets:insert(RouterTable, {H, DestPid}),
    insertToTableHelper(ControllerTable, RouterTable, Dest, T).


% helper function to insert a tuple in lookup_table 
% insertToTable (Data, RouterTable)
insertToTable (_, _, []) ->
    ok;
insertToTable (ControllerTable, RouterTable, [H|T]) ->
    DataToList = tuple_to_list(H),
    Dest = lists:nth(1, H),
    Names = lists:nth(2, H),
    insertToTableHelper(ControllerTable, RouterTable, Dest, Names),
    insertToTable(ControllerTable, RouterTable, T).


initialiseNodes (Graph, [], ControllerTable) -> ok;
initialiseNodes (Graph, [H|T], ControllerTable) ->
  Data = ets:lookup(ControllerTable, H),
  DataItem = lists:nth(1, Data),
  DataItemLs = tuple_to_list(DataItem),
  NodeName = lists:nth(1, DataItemLs),
  RouterPid = lists:nth(2, DataItemLs),
  RouterPid ! {control, self(), self(), 0, 
  fun (Name, Table) ->
    Edges = getEdgesbyNodeName(NodeName),
    insertToTable(ControllerTable, RouterTable, Edges),
    assignEdges(Name, Table, Graph, 0),
    [] % no processes spawned
  end }, 
  initialiseNodes(Graph, T, ControllerTable).

graphToNetwork (Graph) ->
  % first start all the nodes in the graph using router:start/1
  % store the pid of all the nodes started in an ets
  % finally, send control message to the routers with seqNo 0

  ControllerTable = ets:new(router_info, [set]),
  Nodes = getNodes(Graph, []),
  startNodes (Nodes, ControllerTable) % this stores the pid of the nodes 
  initialiseNodes (Graph, Nodes, ControllerTable), % sends control message to the newly started
                                                   % nodes with seqNo. 0
  
  %%%%%%%%%%%%%%%% Getting the first router Info %%%%%%%%%%%%%
  FirstNode = tuple_to_list(lists:nth(1, Graph)),
  FirstNodeName = lists:nth(1, FirstNode),
  Data = ets:lookup(ControllerTable, H),
  DataItem = lists:nth(1, Data),
  DataItemLs = tuple_to_list(DataItem),
  NodePid = lists:nth(2, DataItemLs).


simpleNetworkGraph () ->
  [{red  , [{white, [white, green]},
	    {blue , [blue]}]},
   {white, [{red, [blue]},
	    {blue, [green, red]}]},
   {blue , [{green, [white, green, red]}]},
   {green, [{red, [red, blue, white]}]}
  ].

 








