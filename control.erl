-module(control).
-export([graphToNetwork/1, extendNetwork/4]).

assignEdgesHelper(Name, [], Acc) -> Acc;
assignEdgesHelper(Name, [H|T], Acc) ->

  Data = tuple_to_list(H),

  Node = lists:nth(1, Data),
  if 
    Node == Name -> 
      assignEdgesHelper(Name, T, Acc+1);
    true -> assignEdgesHelper(Name, T, Acc) 
  end.

assignEdges(Name, Table, [], Res) -> Res;
                        % Graph
assignEdges(Name, Table, [H|T], Res) -> 
  Data = tuple_to_list(H),
  Result = assignEdgesHelper(Name, lists:nth(2,Data), Res),
  assignEdges(Name, Table, T, Result).

getNodes([], Nodes) -> Nodes; 
getNodes([H|T], Nodes) -> 
  Data = tuple_to_list(H),
  Node = lists:nth(1,Data),
  getNodes(T, Nodes ++ [Node]).

startNodes([], controller_table) -> ok;
startNodes([H|T], controller_table) -> 
  RouterPid = router:start(H),
  ets:insert(controller_table, {H, RouterPid}),
  startNodes(T, controller_table).

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

% helper function to unpack the list
insertToTableHelper (_, _, _, []) -> ok;
insertToTableHelper (controller_table, RouterTable, Dest, [H|T]) ->
    DestPidLookup = ets:lookup(controller_table, Dest),
    Item = lists:nth(1, DestPidLookup),
    ItemLs = tuple_to_list(Item),
    DestPid = lists:nth(2, ItemLs),
    ets:insert(RouterTable, {H, DestPid}),
    insertToTableHelper(controller_table, RouterTable, Dest, T).


% insertToTable (Data, RouterTable)
insertToTable (_, _, [], _) ->
    ok;                                              % flag to indicate call from extend or graphtonet 
insertToTable (controller_table, RouterTable, [H|T], Flag) ->
    DataToList = tuple_to_list(H),
    % this is a call from graphToNetwork
    if Flag == true ->
      Dest = lists:nth(1, DataToList),
      Names = lists:nth(2, DataToList),
      insertToTableHelper(controller_table, RouterTable, Dest, Names),
      insertToTable(controller_table, RouterTable, T, Flag);
    % call from extendNetwork
    true ->
      DestPid = lists:nth(1, DataToList),
      Dest = getKeyByValue(DestPid, controller_table),
      Names = lists:nth(2, DataToList),
      insertToTableHelper(controller_table, RouterTable, Dest, Names),
      insertToTable(controller_table, RouterTable, T, Flag)
    end.
    


initialiseNodes (Graph, [], controller_table) -> ok;
initialiseNodes (Graph, [H|T], controller_table) ->
  Data = ets:lookup(controller_table, H),
  DataItem = lists:nth(1, Data),
  DataItemLs = tuple_to_list(DataItem),
  NodeName = lists:nth(1, DataItemLs),
  RouterPid = lists:nth(2, DataItemLs),
  RouterPid ! {control, self(), self(), 0, 
  fun (Name, Table) ->
    Edges = getEdgesbyNodeName(Graph, NodeName, []),
    insertToTable(controller_table, Table, Edges, true),
    Result = assignEdges(Name, Table, Graph, 0),
    ets:insert(Table, {'$NoInEdges', Result}),
    [] % no processes spawned
  end }, 
  receive 
    {committed, From, SeqNum} ->
      true;
    {abort, From, SeqNum} ->
      false
  end,
  initialiseNodes(Graph, T, controller_table).

graphToNetwork (Graph) ->
  % first start all the nodes in the graph using router:start/1
  % store the pid of all the nodes started in an ets
  % finally, send control message to the routers with seqNo 0
  try ets:new(controller_table, [named_table, public]) of
    _ -> ok
  catch
    error:Error -> 
        ets:delete(controller_table),
        ets:new(controller_table, [named_table, public])
  end,
  Nodes = getNodes(Graph, []),
  startNodes (Nodes, controller_table), % ControllerTable stores the pid of the nodes 
  initialiseNodes (Graph, Nodes, controller_table), % sends control message to the newly started
                                                   % nodes with seqNo. 0
  
  %%%%%%%%%%%%%%%% Getting the first router Info %%%%%%%%%%%%%
  FirstNode = tuple_to_list(lists:nth(1, Graph)),
  FirstNodeName = lists:nth(1, FirstNode),
  Data = ets:lookup(controller_table, FirstNodeName),
  DataItem = lists:nth(1, Data),
  DataItemLs = tuple_to_list(DataItem),
  NodePid = lists:nth(2, DataItemLs).

getValueByKey(Key, Table) ->
  Data = ets:lookup(Table, Key),
  DataItem = lists:nth(1, Data),
  DataItemLs = tuple_to_list(DataItem),
  NodePid = lists:nth(2, DataItemLs).

getKeyByValue(Value, Table) ->
  KeyLs = ets:match(Table, {'$1', Value}),
  Key = lists:nth(1, lists:nth(1, KeyLs)),
  Key.

calculateNewInEdges(Name, [], Acc) -> Acc;
calculateNewInEdges(Name, [H|T], Acc) ->

  Data = tuple_to_list(H),

  NodePid = lists:nth(1, Data),
  NodeName = getKeyByValue(NodePid, controller_table),
  if 
    NodeName == Name -> 
      calculateNewInEdges(Name, T, Acc+1);
    true -> calculateNewInEdges(Name, T, Acc) 
  end.

extendNetwork (RootPid, SeqNum, From, {NodeName, Edges}) ->
  RootPid ! {control, self(), self(), SeqNum, 
  fun(Name, Table) ->
    if Name == From ->
      % start the new router process
      RouterPid = router:start(NodeName),
      ets:insert(controller_table, {NodeName, RouterPid}),
      RouterPid ! {control, self(), self(), 0, 
      fun (RouterName, RouterTable) ->
        insertToTable(controller_table, RouterTable, Edges, false),
        ets:insert(RouterTable, {'$NoInEdges', 1}),  
        [] % no processes spawned
      end }, 
      % make temporary changes to the router
      NewNodePid =  getValueByKey(NodeName, controller_table),
      ets:insert(Table, {NodeName, NewNodePid}),
      NewInEdges = calculateNewInEdges(Name, Edges, 0),
      ExistingEdges = getValueByKey('$NoInEdges', Table),
      ets:insert(Table, {'$NoInEdges', ExistingEdges+NewInEdges}),  
      [NewNodePid];

    true ->
      % make temporary changes to the router table
      ForwardPid =  getValueByKey(From, Table),
      ets:insert(Table, {NodeName, ForwardPid}),
      NewInEdges = calculateNewInEdges(Name, Edges, 0),
      ExistingEdges = getValueByKey('$NoInEdges', Table),
      ets:insert(Table, {'$NoInEdges', ExistingEdges+NewInEdges}),  
      [] % no processes spawned
     end
  end},
  receive
    % start receiving from RootPid
    % if comitted then return true, else false
    % need to kill the new process if aborted
    {committed, FromPid, SeqNum} ->
      true;
    {abort, FromPid, SeqNum} ->
      % need to check if the process was spawned
      % if yes, kill the process
      % revert the controller table for consistency
      Match = ets:match(controller_table, {NodeName, '$1'}),
      if Match =/= [] ->
        % indicated new process has been spawned so need to kill it
        NodePid = lists:nth(1, lists:nth(1, Match)),
        NodePid ! stop,
        ets:match_delete(controller_table, {NodeName, '$1'}),
        false;
      true ->
        % node not spawned so do nothing return false
        false
      end
  end.