-module(falling).
-include("../include/fgame.hrl").
-export([new_game/1, play_card/3, draw_card/3, deal/1]).

new_game(PlayerCount) ->
  % Remove Me
  random:seed(now()),
  Deck = generate_deck(),
  Shuffled = shuffle_deck(Deck, []),
  #fgame{deck=Shuffled, players = add_players(PlayerCount, [])}.

add_players(0, Players) ->
  Players;
add_players(Count, Players) ->
  Player = #fplayer{id=Count},
  add_players(Count - 1, [add_stack(Player) | Players]).

remove_index(Front, [_ | List], 1) ->
  lists:append(Front, List);
remove_index(Front, List, Index) when Index > 1->
  remove_index(lists:append(Front, [hd(List)]), tl(List), Index - 1).

shuffle_deck([], DeckOut) ->
  DeckOut;
shuffle_deck(DeckIn, DeckOut) ->
  Index = random:uniform(length(DeckIn)),
  Card = lists:nth(Index, DeckIn),
  shuffle_deck( remove_index([], DeckIn, Index), [ Card | DeckOut]).

add_card(_Card, 0, Deck) ->
  Deck;
add_card(Card, Number, Deck) ->
  add_card(Card, Number - 1, [ Card | Deck ]).

generate_deck() ->
  Skips= add_card(skip, 12, []),
  Hits = add_card(hit, 12, Skips),
  Splits = add_card(split, 5, Hits),
  Extras = add_card(extra, 5, Splits),
  Pushes = add_card(push, 4, Extras),
  Grabs = add_card(grab, 5, Pushes),
  add_card(stop, 6, Grabs).

%Skip:   12
%Hit:    12
%Split:  5
%Extra:  5
%Push:   4
%Grab:   5
%Stop:   6
%Ground: 5

%externally facing game state update funcitons return a tuple of { game, delta list }
% A delta list is a tuple which contains :
%{ card move, {player, stack} or deck, {player, stack} or trash, card }
add_stacks(0, Player) ->
  Player;
add_stacks(Count, Player) ->
  add_stacks(Count -1, add_stack(Player)).

add_stack(Player) ->
  Stacks = Player#fplayer.stacks,
  StackId = Player#fplayer.stack_id,
  Player#fplayer{stacks= [#fstack{id=StackId} | Stacks], stack_id=StackId + 1}.

get_stack(StackId, Player) ->
  lists:keyfind(StackId, #fstack.id, Player#fplayer.stacks).
set_stack(StackId, Player, Stack) ->
  Player#fplayer{stacks=lists:keystore(StackId, #fstack.id, Player#fplayer.stacks, Stack)}.

get_player(PlayerId, Game) ->
  lists:keyfind(PlayerId, #fplayer.id, Game#fgame.players).
set_player(PlayerId, Game, Player) ->
  Game#fgame{players=lists:keystore(PlayerId, #fplayer.id, Game#fgame.players, Player)}.

deal(Game) ->
  case Game#fgame.deal_stage of
    rider ->
      handle_rider(Game);
    hits when length(Game#fgame.deck) > 0->
      Target=get_player(Game#fgame.deal_player, Game),
      [Card | Remain] = Game#fgame.deck,
      Stack = get_stack(Game#fgame.deal_stack,Target),
      Dealt = set_stack(Game#fgame.deal_stack, Target, Stack#fstack{cards=[Card|Stack#fstack.cards]}),
      DealtGame=set_player(Game#fgame.deal_player, Game, Dealt),
      setup_deal(DealtGame#fgame{hit_count=DealtGame#fgame.hit_count + 1, deck=Remain});
    hits ->
      Target=get_player(Game#fgame.deal_player, Game),
      NextGame = case Target#fplayer.hand of
        stop ->
          DealtPlayer = Target#fplayer{hand=none},
          DealtGame=set_player(Game#fgame.deal_player, Game, DealtPlayer),
          setup_deal(DealtGame#fgame{hit_count=DealtGame#fgame.hit_count + 1});
        _OtherCard ->
          DeadPlayer = Target#fplayer{dead=true},
          DealtGame=set_player(Game#fgame.deal_player, Game, DeadPlayer),
          setup_deal_player(DealtGame)
      end,
      case get_winner(NextGame) of
        none ->
          NextGame;
        Winner ->
          {game_over, Winner, NextGame}
      end
  end.

get_winner(Game) ->
  % TODO go through all the players and find a not dead one
  none.

cleanup_stacks(Player) ->
  FirstStack = hd(Player#fplayer.stacks),
  CleanedStacks = [ Stack || Stack <- Player#fplayer.stacks, length(Stack#fstack.cards) > 0 ],
  NextStacks = case CleanedStacks of
    [] ->
      [FirstStack];
    Stacks ->
      Stacks
  end,
  Player#fplayer{stacks=NextStacks}.
      
      

handle_rider(Game) ->
  Player = get_player(Game#fgame.deal_player, Game),
  NoRider = Player#fplayer{rider=none, extras=0},
  Clean = cleanup_stacks(NoRider),
  NextGame = set_player(Player#fplayer.id, Game, Clean),
  RiderHandled = case Player#fplayer.rider of
    none ->
      NextGame;
    hit ->
      NextGame#fgame{hit_max = 2 + Player#fplayer.extras};
    skip when length(NextGame#fgame.deck) > 0 ->
      setup_deal_player(NextGame);
    skip ->
      NextGame;
    split ->
      SplitPlayer = add_stacks(1+Player#fplayer.extras, Clean),
      set_player(Player#fplayer.id, NextGame, SplitPlayer)
  end,
  RiderHandled#fgame{deal_stage=hits}.
    

setup_deal(Game) ->
  case Game#fgame.hit_count of
    Hits when Hits < Game#fgame.hit_max ->
      Game#fgame{hit_count = Hits + 1};
    _MaxHits ->
      setup_deal_stack(Game)
  end.

%TODO make sure the player isn't dead
next_player(CurPlayerNum, GamePlayers) ->
  NextPlayerNum = CurPlayerNum +1,
  case GamePlayers of
    Players when length(Players) >= NextPlayerNum ->
      NextPlayerNum;
    _Players ->
      io:format("Next Stack: ~w~n", [NextPlayerNum]),
      io:format("Stacks length: ~w~n", [length(GamePlayers)]),
      1
  end.

next_stack(CurStackNum, Player) ->
  NextStackNum = CurStackNum + 1,
  io:format("Next Stack: ~w~n", [NextStackNum]),
  io:format("Stacks length: ~w~n", [length(Player#fplayer.stacks)]),
  case Player#fplayer.stacks of
    Stacks when length(Stacks) >= NextStackNum ->
      NextStackNum;
    _Stacks ->
      none
  end.

setup_deal_stack(Game) ->
  Target=get_player(Game#fgame.deal_player, Game),
  NextStack = next_stack(Game#fgame.deal_stack, Target),
  case NextStack of
    none ->
      setup_deal_player(Game);
    StackNum ->
      Game#fgame{deal_stack=StackNum, hit_count=0}
  end.

setup_deal_player(Game) ->
  NextPlayerNum = next_player(Game#fgame.deal_player, Game#fgame.players),
  NextPlayer = get_player(NextPlayerNum, Game),
  TopStack = hd(NextPlayer#fplayer.stacks),
  Game#fgame{deal_player=NextPlayerNum, hit_count=0, deal_stack=TopStack#fstack.id, deal_stage=rider}.

draw_card(PlayerId, StackId, Game) ->
  Player = get_player(PlayerId, Game),
  case Player#fplayer.hand of
    none ->
      Stack = get_stack(StackId, Player),
      [Card | Remain] = Stack#fstack.cards,
      NewStackPlayer = set_stack(StackId,Player, Stack#fstack{cards=Remain}),
      DrawPlayer = NewStackPlayer#fplayer{hand=Card},
      set_player(PlayerId, Game, DrawPlayer);
    _SomeCard ->
      Game
  end.
  
play_card(FromPlayerId, ToPlayerId, Game) ->
  FromPlayer = get_player(FromPlayerId, Game),
  case FromPlayer#fplayer.hand of
    none ->
      Game;
    Card when FromPlayerId == ToPlayerId ->
      NextFromPlayer = do_card(Card, FromPlayer, on_self),
      set_player(FromPlayerId, Game, NextFromPlayer);
    Card ->
      ToPlayer = get_player(ToPlayerId, Game),
      {NextFromPlayer, NextToPlayer} = do_card(Card, FromPlayer, ToPlayer),
      FromUpdate = set_player(FromPlayerId, Game, NextFromPlayer),
      set_player(ToPlayerId, FromUpdate, NextToPlayer)
    end.

do_card(grab, Player, on_self) ->
  Player;
do_card(grab, FromPlayer, ToPlayer) ->
  case FromPlayer#fplayer.rider of
    none ->
      case ToPlayer#fplayer.rider of
        none ->
          {FromPlayer, ToPlayer};
        Rider ->
          { FromPlayer#fplayer{hand=none, rider=Rider},
            ToPlayer#fplayer{rider=none} }
      end;
    _Rider ->
      {FromPlayer, ToPlayer}
  end;
do_card(push, Player, on_self) ->
  Player;
do_card(push, FromPlayer, ToPlayer) ->
  case ToPlayer#fplayer.rider of
    none ->
      case FromPlayer#fplayer.rider of
        none ->
          { FromPlayer, ToPlayer };
        Rider ->
          { FromPlayer#fplayer{hand=none, rider=none},
            ToPlayer#fplayer{rider=Rider} }
      end;
    _Rider ->
      {FromPlayer, ToPlayer}
  end;
do_card(stop, Player, on_self) ->
  case Player#fplayer.rider of
    none ->
      Player;
    _Rider ->
      Player#fplayer{hand=none, rider=none}
  end;
do_card(stop, FromPlayer, ToPlayer) ->
  case FromPlayer#fplayer.rider of
    none ->
      {FromPlayer, ToPlayer};
    _Rider ->
      {FromPlayer#fplayer{hand=none}, ToPlayer#fplayer{rider=none}}
  end;
do_card(extra, Player, on_self) ->
  case Player#fplayer.rider of
    none ->
      Player;
    _Rider ->
      Player#fplayer{extras=Player#fplayer.extras + 1}
  end;
do_card(extra, FromPlayer, ToPlayer) ->
  case ToPlayer#fplayer.rider of
    none ->
      {FromPlayer, ToPlayer};
    _Rider ->
      {FromPlayer#fplayer{hand=none}, ToPlayer#fplayer{extras=ToPlayer#fplayer.extras + 1}}
  end;
do_card(Rider, Player, on_self) ->
  case Player#fplayer.rider of
    none ->
      Player#fplayer{hand = none, rider = Rider};
    _Rider ->
      Player
  end;
do_card(Rider, FromPlayer, ToPlayer) ->
  case ToPlayer#fplayer.rider of
    none ->
      {FromPlayer#fplayer{hand=none}, ToPlayer#fplayer{rider=Rider}};
    _Rider ->
      {FromPlayer, ToPlayer}
  end.

