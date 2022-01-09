-module(router).
-export([start/1]).

start(RouterName) -> 
    % initialise the lookup router table
    RouterTable = ets:new(lookup_table, [set, public]),
    Pid = spawn(fun() -> listen(RouterName, RouterTable, none, none, none, 0) end),
    Pid.

getValueByKey(Key, Table) ->
  Data = ets:lookup(Table, Key),
  DataItem = lists:nth(1, Data),
  DataItemLs = tuple_to_list(DataItem),
  NodePid = lists:nth(2, DataItemLs).

getNoInEdges(Table) ->
    Total = getValueByKey('$NoInEdges', Table),
    Total.

getImmediateNeighboursHelper(Table, '$end_of_table', Neighbours) -> Neighbours;
getImmediateNeighboursHelper(Table, Key, Neighbours) ->
    NodePid = getValueByKey(Key, Table), %get the pid of the immediate neighbour from the router's table
    NextKey = ets:next(Table, Key),
    Check = lists:member(NodePid, Neighbours),
    if (Check == false) and (Key =/= '$NoInEdges')->
        NewNeighbour = lists:append(Neighbours, [NodePid]),
        getImmediateNeighboursHelper(Table, NextKey, NewNeighbour);
    true -> 
        getImmediateNeighboursHelper(Table, NextKey, Neighbours)
    end.

% returns the list of immediate neighbours form a router's table
getImmediateNeighbours(Table) ->
  FirstKey = ets:first(Table),
  Neighbours = getImmediateNeighboursHelper(Table, FirstKey, []),
  Neighbours.

% propagates doCommit to its immediate neighbours
% returns how many doCommits received from incoming edges
propagateDoCommit([], SeqNum, Flag, Total) -> Total;
propagateDoCommit([NodePid|T], SeqNum, Flag, Total) ->
    if Flag == true ->
        NodePid ! {doCommit, self(), SeqNum},
        ok;
    true ->
        ok
    end,
    receive 
        {doCommit, From, SeqNumReceived} ->
            % request from incoming edge
            From ! {committed, self(), SeqNum},
            propagateDoCommit([NodePid|T], SeqNum, false, Total + 1);       
        {committed, From, SeqNumReceived} ->
            propagateDoCommit(T, SeqNum, true, Total)
    end.

propagateDoRollBack([], _, _, Total) -> Total;
propagateDoRollBack([NodePid|T], CurrSeqNum, From, Total) ->
    if From == none ->
        NodePid ! {rollBack, self(), CurrSeqNum},
        receive
        {aborted, FromPid, SeqNumReceived} ->
            % move on to the next child
            propagateDoRollBack(T, CurrSeqNum, none, Total);
        {rollBack, FromPid, SeqNumReceived} ->
            FromPid ! {aborted, self(), CurrSeqNum},
            propagateDoRollBack([NodePid|T], CurrSeqNum, none, (Total+1))
        end;
    true ->
        NodePid ! {rollBack, From, CurrSeqNum},
        propagateDoRollBack(T, CurrSeqNum, From, Total)   
    end.
    
propagateControlRequest([], _, _,_,_,_,_, Commit) -> 
    Commit;
                                                                                    % flag to receive or send control msg
propagateControlRequest([NodePid|T], Pid, From, SeqNum, ControlFun, CurrSeqNum, Flag, Commit) ->
    if Flag == true ->
        NodePid ! {control, self(), Pid, SeqNum, ControlFun},
        ok;
    true ->
        ok
    end,
    receive
        {canCommit, NodeId, SeqNum} ->
            propagateControlRequest(T, Pid, From, SeqNum, ControlFun, CurrSeqNum, true, true);
        {abort, NodeId, SeqNum} ->
            propagateControlRequest([], Pid, From, SeqNum, ControlFun, CurrSeqNum, Flag, false); % signals abort with a false and ends the recursion
        {control, FromPid, Pid, SeqNum, ControlFun} ->
            if 
            % if the router has already received a control req for this seqNum, don't propagate again (don't call control Fun)
            CurrSeqNum == SeqNum ->
                FromPid ! {canCommit, self(), SeqNum},
                propagateControlRequest([NodePid|T],Pid, From, SeqNum, ControlFun, CurrSeqNum, false, true);
            true ->
                % engaged in 2PC
                % abort this request
                FromPid ! {abort, self(), SeqNum},
                propagateControlRequest([NodePid|T],Pid, From, SeqNum, ControlFun, CurrSeqNum, false, true)
            end
    after 5000 ->
        From ! {abort, self(), SeqNum},
        false
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

% The message loop
listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, Phase2SeqNum, TotalReceived) ->
    receive
        {control, From, Pid, SeqNum, ControlFun} ->
            if  SeqNum == 0 -> 
                Children = ControlFun(RouterName, RouterTable),
                if 
                    Children =/= abort ->
                        Pid ! {committed, self (), SeqNum};
                true -> 
                    Pid ! {abort, self (), SeqNum}
                end,
                listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, Phase2SeqNum, TotalReceived);
            true ->
                % indicates the router is in second phase of a different commit so abort
                if (Phase1SeqNum =/= none) and (Phase1SeqNum =/= SeqNum) ->   
                    From ! {abort, self(), SeqNum},
                    listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, Phase2SeqNum, TotalReceived);
                true ->
                    if Phase1SeqNum == none -> 
                        % need to clone the table
                        TempTable = duplicateTable(RouterTable),
                        Children = ControlFun(RouterName, TempTable),
                        if 
                            Children =/= abort ->
                                % need to propagate control request to immediate neighbours
                                Neighbours = getImmediateNeighbours(RouterTable),
                                TableDump = ets:match(RouterTable, '$1'),
                                Result = propagateControlRequest(Neighbours, Pid, self(), SeqNum, ControlFun, SeqNum, true, true),
                                % canCommit
                                if Result == true -> 
                                    if From == Pid->
                                        % need to start second phase of 2PC
                                        %%%%%%%%%%%%%%%%%%%%%%%% Second Phase for coordinator Starts here %%%%%%%%%%%%
                                        
                                        propagateDoCommit(Neighbours, SeqNum, true, 1), % coordinator does not need to check InEdges
                                        Pid ! {committed, self(), SeqNum},
                                        % 2PC complete start listening with the new table for new control req
                                        listen(RouterName, TempTable, none, none, none, 0);
                                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                    true -> 
                                        From ! {canCommit, self(), SeqNum},
                                        % start listening for second phase of 2PC with the same sequence number
                                        listen(RouterName, RouterTable, TempTable, SeqNum, none, 0)
                                    end;
                                % abort
                                true -> 
                                    if From == Pid ->
                                        % initiate the rollback
                                        propagateDoRollBack(Neighbours, SeqNum, none, 1),
                                        Pid ! {abort, self(), SeqNum}, % abort 2PC by sending abort to controller
                                        
                                        %2PC complete start listening for new control requests
                                        listen(RouterName, RouterTable, none, none, none, 0);
                                    true ->
                                        From ! {abort, self(), SeqNum},
                                        % now listen again and propagate rollBack requests to all the neighbours
                                        listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, none, 0)
                                    end
                                end;
                        true -> 
                            From ! {abort, self (), SeqNum},
                            % control fun failed start listening for new control requests
                            listen(RouterName, RouterTable, none, none, none, 0)                 
                        end;
                    % if receives control req with same seqNum
                    true -> 
                        From ! {canCommit, self(), SeqNum},
                        listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, Phase2SeqNum, TotalReceived)
                    end
                end
                    
            end;

        {doCommit, FromNid, SeqNum} ->
            NoInEdges = getNoInEdges(RouterTable),
            if SeqNum == Phase2SeqNum ->
                % received doCommit from incoming edge 
                if (TotalReceived + 1) ==  NoInEdges ->
                    % end of 2PC
                    FromNid ! {committed, self(), SeqNum},
                    listen(RouterName, TemporaryTable, none, none, none, 0);
                true ->
                    listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, SeqNum, TotalReceived + 1)
                end;
            true -> % hasn't received a doCommit so propagate to the neighbours.
                Neighbours = getImmediateNeighbours(RouterTable),   
                FromInEdges = propagateDoCommit(Neighbours, SeqNum, true, 1),
                FromNid ! {committed, self(), SeqNum},
                % if hasn't received doCommit from all incoming edges, keep listening with the same seq num
                if FromInEdges =/=  NoInEdges ->
                    % second phase sequence Num initialised here
                    listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, SeqNum, FromInEdges);
                true ->
                    % done with 2PC now
                    listen(RouterName, TemporaryTable, none, none, none, 0)
                end
            end;
        
        {rollBack, From, SeqNum} ->
            NoInEdges = getNoInEdges(RouterTable),
            % router hasn't engaged in 2PC, still need to propagate to the neighbours for completion of 2PC and consistency
            if Phase1SeqNum == none ->
                % just propagate the req no need to roll the table over cause none changed
                Neighbours = getImmediateNeighbours(RouterTable),
                FromInEdges = propagateDoRollBack(Neighbours, SeqNum, none, 1), 
                From ! {aborted, self(), SeqNum},
                if FromInEdges =/=  NoInEdges ->
                    % second phase sequence Num initialised here
                    listen(RouterName, RouterTable, TemporaryTable, SeqNum, SeqNum, FromInEdges);
                true ->
                    % done with 2PC now
                    listen(RouterName, RouterTable, none, none, none, 0)
                end;
            true -> 
                % different rollback request, just propagate it , self() is from
                if (Phase1SeqNum =/= none) and (SeqNum =/= Phase1SeqNum) ->
                    Neighbours = getImmediateNeighbours(RouterTable),
                    propagateDoRollBack(Neighbours, SeqNum, From, 0),
                    listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, Phase2SeqNum, TotalReceived);
                % same seqNum
                true ->
                    % received rollBack for the first time
                    if (Phase2SeqNum == none) and (Phase1SeqNum == SeqNum) ->
                        % propagate rollback to immediate neighbours
                        Neighbours = getImmediateNeighbours(RouterTable),
                        FromInEdges = propagateDoRollBack(Neighbours, SeqNum, none, 1),    
                        From ! {aborted, self(), SeqNum},
                        if FromInEdges =/=  NoInEdges ->
                            % second phase sequence Num initialised here
                            listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, SeqNum, FromInEdges);
                        true ->
                            % done with 2PC now
                            % discard the temporary table
                            listen(RouterName, RouterTable, none, none, none, 0)
                        end;
                    true ->
                        if (TotalReceived + 1) ==  NoInEdges ->
                            % end of 2PC
                            listen(RouterName, RouterTable, none, none, none, 0);
                        true ->
                            listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, SeqNum, TotalReceived + 1)
                        end
                    end                   
                end
            end;

        {message, Dest, From, Pid, Trace} ->
            if 
                Dest == RouterName ->
                    NewTrace = lists:append(Trace, [RouterName]),
                    Pid ! {trace, self(), NewTrace};
            true ->
                NextPidLookup = ets:lookup(RouterTable, Dest),
                NextPidItem = lists:nth(1, NextPidLookup),
                NextPidItemLs = tuple_to_list(NextPidItem),
                NextPid = lists:nth(2, NextPidItemLs),
                NewTrace = lists:append(Trace, [RouterName]),
                NextPid ! {message, Dest, self(), Pid, NewTrace}
            end,
            listen(RouterName, RouterTable, none, none, none, TotalReceived);
        
        {dump, From} ->
			MatchDump = ets:match (RouterTable, '$1'),
			From ! {table, self(), MatchDump},
			listen(RouterName, RouterTable, TemporaryTable, Phase1SeqNum, Phase2SeqNum, TotalReceived);

		stop ->
            % only stop if not engaged in 2PC
			if Phase1SeqNum == none ->
                ets:delete(RouterTable),
				exit(1)
			end
    end.

