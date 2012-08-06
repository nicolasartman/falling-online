-module(falling_instance).
-behavior(gen_server).
%-include("include/fgame.hrl").
-include("include/game_meta.hrl").
-export([start_link/0]).
-export([new_instance/0, send_deal/2]).
-export([init/1, handle_call/3]).

new_instance() ->
  {ok, Pid} = gen_server:start_link(falling_instance, [], []),
  Pid.
  

start_link() ->
  gen_server:start_link(falling_instance, [], []).

init([]) ->
  {ok, {#game_meta{}, none}}.

handle_call({start_game, Speed, Count}, _From, {Meta, none}) ->
  Game = falling:new_game(Count),
  timer:apply_after(Speed, ?MODULE, send_deal, [self(), Speed]),
  {reply, ok, {Meta, Game}};
handle_call(state, _From, {Meta, Game}) ->
  {reply, Game, {Meta, Game}};
handle_call(_Any, _From, {Meta, {game_over, Winner, Game}}) ->
  {reply, {game_over, Winner, []}, {Meta, {game_over, Winner, Game}}};
handle_call(deal, _From, {Meta, Game}) ->
  case falling:deal(Game) of
    {NextGame, Deltas} ->
      broadcast_deltas(Meta, Deltas),
      {reply, Deltas, {Meta, NextGame}};
    { game_over, Winner, Deltas, NextGame } ->
      broadcast_deltas(Meta, {game_over, Winner, Deltas}),
      {reply, {game_over, Winner, Deltas}, {Meta, {game_over, Winner, NextGame}}}
  end;
handle_call({draw, PlayerId, StackId}, _From, {Meta, Game}) ->
  {NextGame, Deltas} = falling:draw_card(PlayerId, StackId, Game),
  broadcast_deltas(Meta, Deltas),
  {reply, Deltas, {Meta, NextGame}};
handle_call({play_card, FromPlayerId, ToPlayerId}, _From, {Meta, Game}) ->
  { NextGame, Deltas } = falling:play_card(FromPlayerId, ToPlayerId, Game),
  broadcast_deltas(Meta, Deltas),
  {reply, Deltas, {Meta, NextGame}}.

send_deal(GamePid, Speed) ->
  case gen_server:call(GamePid, deal) of
    {game_over, Winner, _Deltas} ->
      io:format("Game over, player ~w wins~n", [Winner]),
      ok;
    DeltaList ->
      io:format("Card dealt ~w~n", [DeltaList]),
      timer:apply_after(Speed, ?MODULE, send_deal, [GamePid, Speed])
  end.

%Don't broadcast rejection messages
broadcast_deltas(_Meta, bad_move) ->
  ok;
broadcast_deltas(Meta, DeltaList) ->
  lists:foreach(fun (Listen) -> Listen ! DeltaList end, Meta#game_meta.listeners).
