-module(falling_instance).
-behavior(gen_server).
%-include("include/fgame.hrl").
-include("include/game_meta.hrl").
-export([start_link/0]).
-export([new_instance/0]).
-export([init/1, handle_call/3, handle_info/2]).

new_instance() ->
  {ok, Pid} = gen_server:start_link(falling_instance, [], []),
  Pid.
  

start_link() ->
  gen_server:start_link(falling_instance, [], []).

init([]) ->
  {ok, {#game_meta{}, none}}.

handle_call({start_game, Speed, Count}, _From, {Meta, none}) ->
  Game = falling:new_game(Count),
  erlang:send_after(Speed, self(), {send_deal, Speed}),
  {reply, ok, {Meta, Game}};
handle_call({join, Nickname}, {From, _Tag}, {Meta, none}) ->
  {PlayerId, NewMeta} = add_player_to_game(From, Nickname, Meta),
  {reply, {PlayerId, nickname_list(NewMeta)}, {NewMeta, none}};
handle_call({join, _Nickname}, _From, {Meta, Game}) ->
  {reply, {error, already_started}, {Meta, Game}};
handle_call(listen, {From, _Tag}, {Meta, Game}) ->
  NewMeta = add_spectator_to_game(From, Meta),
  {reply, nickname_list(NewMeta), NewMeta, Game};
handle_call(state, _From, {Meta, Game}) ->
  {reply, Game, {Meta, Game}};
handle_call(_Any, _From, {Meta, {game_over, Winner, Game}}) ->
  {reply, {game_over, Winner, []}, {Meta, {game_over, Winner, Game}}};
handle_call(deal, _From, {Meta, Game}) ->
  case do_deal(Game, Meta) of
    {game_over, Winner, Deltas, NextGame} ->
      {reply, {game_over, Winner, Deltas}, {Meta, {game_over, Winner, NextGame}}};
    {NextGame, Deltas} ->
      {reply, Deltas, {Meta, NextGame}}
  end;
handle_call({draw, PlayerId, StackId}, _From, {Meta, Game}) ->
  {NextGame, Deltas} = falling:draw_card(PlayerId, StackId, Game),
  broadcast_deltas(Meta, Deltas),
  {reply, Deltas, {Meta, NextGame}};
handle_call({play_card, FromPlayerId, ToPlayerId}, _From, {Meta, Game}) ->
  { NextGame, Deltas } = falling:play_card(FromPlayerId, ToPlayerId, Game),
  broadcast_deltas(Meta, Deltas),
  {reply, Deltas, {Meta, NextGame}}.

nickname_list(#game_meta{players=Players}) ->
  lists:foldl(fun(#player_meta{nickname=Nickname}, Names) -> [Nickname|Names] end, [], Players).

add_spectator_to_game(Pid, #game_meta{listeners=Listeners} = Meta) ->
  Meta#game_meta{listeners = [Pid | Listeners]}.

add_player_to_game(Pid, Nickname, #game_meta{cur_id=CurId, players=Players} = Meta) ->
  { CurId, Meta#game_meta{players=[#player_meta{id=CurId, nickname=Nickname, pid=Pid} | Players],
                 cur_id = CurId + 1}}.

handle_info({send_deal, Speed}, {Meta, Game}) ->
  case do_deal(Game, Meta) of
    { game_over, Winner, Deltas, NextGame } ->  
      io:format("Card dealt ~w~n", [Deltas]),
      io:format("Game over, player ~w wins~n", [Winner]),
      { noreply, {Meta, {game_over, Winner, NextGame}}};
    {NextGame, Deltas} ->
      io:format("Card dealt ~w~n", [Deltas]),
      erlang:send_after(Speed, self(), {send_deal, Speed}),
      { noreply, {Meta, NextGame}}
  end.

do_deal(Game, Meta) ->  
  case falling:deal(Game) of
    { game_over, Winner, Deltas, NextGame } ->
      broadcast_deltas(Meta, {game_over, Winner, Deltas}),
      {game_over, Winner, Deltas, NextGame};
    {NextGame, Deltas} ->
      broadcast_deltas(Meta, Deltas),
      {NextGame, Deltas}
  end.

%Don't broadcast rejection messages
broadcast_deltas(_Meta, bad_move) ->
  ok;
broadcast_deltas(Meta, DeltaList) ->
  lists:foreach(fun (Listen) -> Listen ! DeltaList end, Meta#game_meta.listeners),
  lists:foreach(fun (#player_meta{pid=Pid}) -> Pid ! DeltaList end, Meta#game_meta.players).

