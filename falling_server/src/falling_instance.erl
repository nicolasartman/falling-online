-module(falling_instance).
-behavior(gen_server).
-include("../include/fgame.hrl").
-export([start_link/1]).
-export([init/1, handle_call/3]).

start_link(PlayerCount) ->
  gen_server:start_link(falling_instance, PlayerCount, []).

init(Count) ->
  {ok, falling:new_game(Count)}.

handle_call(state, _From, Game) ->
  {reply, Game, Game};
handle_call(deal, _From, Game) ->
  {NextGame, Deltas} = falling:deal(Game),
  {reply, Deltas, NextGame};
handle_call({draw, PlayerId, StackId}, _From, Game) ->
  {NextGame, Deltas} = falling:draw_card(PlayerId, StackId, Game),
  {reply, Deltas, NextGame};
handle_call({play_card, FromPlayerId, ToPlayerId}, _From, Game) ->
  { NextGame, Deltas } = falling:play_card(FromPlayerId, ToPlayerId, Game),
  {reply, Deltas, NextGame}.
