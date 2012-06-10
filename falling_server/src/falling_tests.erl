-module(falling_tests).
-compile(export_all).

deal_game(N) ->
  deal_game_inner({falling:new_game(N), []}).

deal_game_inner(Game) ->
  io:format("Game state: ~w~n", [Game]),
  case Game of
    {game_over, Winner, Deltas} ->
      io:format("Game over, ~w ~w~n", [Winner, Deltas]);
    {NextGame, _Deltas} ->
      deal_game_inner(falling:deal(NextGame))
  end.
  
