-module(game_of_life).
-export([world_tick/1]).

tick({A,2}) -> A;
tick({_,3}) -> true;
tick({_,_}) -> false.

explode(Coords) ->
    [[{{X+DeltaX, Y+DeltaY}, {Center, if Center -> 0; true -> 1 end}}
      || DeltaX <- [-1,0,1],
         DeltaY <- [-1,0,1],
         Center <- [DeltaX =:= 0 andalso DeltaY =:= 0]]
     || {X,Y} <- Coords].

cell_merge(_K, {A1,N1}, {A2,N2}) -> {A1 or A2, N1+N2}.

world_tick(Coords) ->
    Dict = lists:foldl(fun(D1,D2) -> dict:merge(fun cell_merge/3, D1, D2) end,
                       dict:new(),
                       [dict:from_list(L) || L <- explode(Coords)]),
    dict:fetch_keys(dict:filter(fun(_,V) -> tick(V) end, Dict)).

%% 4 whitespace
%% 17 loc
