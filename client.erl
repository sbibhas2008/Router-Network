-module(client).
-export([client/1]).

% Client code using the increment server

inc (Num) -> 
  Num + 1.

client (Server) ->
Server ! {self (), 10, fun(Num) ->  inc(Num) end},
receive
  {From, Reply} -> io:format ("Result: ~w~n", [Reply])
end.