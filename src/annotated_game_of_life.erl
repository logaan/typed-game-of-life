-module(annotated_game_of_life).
-export([world_tick/1]).

-type coord() :: {integer(), integer()}.
-type cell() :: {boolean(), integer()}.
-type world() :: [[{coord(),cell()}]].

-spec tick(cell()) -> boolean().
tick({A,2}) -> A;
tick({_,3}) -> true;
tick({_,_}) -> false.

-spec explode([coord()]) -> world().
explode(Coords) ->
    [[{{X+DeltaX, Y+DeltaY}, {Center, if Center -> 0; true -> 1 end}}
      || DeltaX <- [-1,0,1],
         DeltaY <- [-1,0,1],
         Center <- [DeltaX =:= 0 andalso DeltaY =:= 0]]
     || {X,Y} <- Coords].

-spec cell_merge(Key::_, cell(), cell()) -> cell().
cell_merge(_K, {A1,N1}, {A2,N2}) -> {A1 or A2, N1+N2}.

-spec world_tick([coord()]) -> [coord()].
world_tick(Coords) ->
    Dict = lists:foldl(fun(D1,D2) -> dict:merge(fun cell_merge/3, D1, D2) end,
                       dict:new(),
                       [dict:from_list(L) || L <- explode(Coords)]),
    dict:fetch_keys(dict:filter(fun(_,V) -> tick(V) end, Dict)).

%% 5 whitespace
%% 24 loc
