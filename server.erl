-module(server).
-export([loop/0, start_server/0]).

loop () ->  
io:format ("Server: ~w~n", [self()]),  
receive
  {start, From} ->
    receive
      {control, Num} ->
        loop()
    end,
  loop ();
  {another, From} ->
    io:format("Caught another later~n");
  stop -> true
end.
% Initiate the server
start_server() -> spawn (fun () -> loop () end).