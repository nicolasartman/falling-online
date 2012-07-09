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
%{ card move, {player, rider or hand} or deck, {player, stack or rider or hand} or trash, card }
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

get_player(PlayerId, PlayerList) ->
  lists:keyfind(PlayerId, #fplayer.id, PlayerList).
set_player(PlayerId, Game, Player) ->
  Game#fgame{players=lists:keystore(PlayerId, #fplayer.id, Game#fgame.players, Player)}.

deal(Game) ->
  case Game#fgame.deal_stage of
    %Deal with rider
    %Must return delta
    rider ->
      handle_rider(Game);
    %Normal card being dealt
    hits when length(Game#fgame.deck) > 0->
      Target=get_player(Game#fgame.deal_player, Game#fgame.players),
      [Card | Remain] = Game#fgame.deck,
      Stack = get_stack(Game#fgame.deal_stack,Target),
      Dealt = set_stack(Game#fgame.deal_stack, Target, Stack#fstack{cards=[Card|Stack#fstack.cards]}),
      DealtGame=set_player(Game#fgame.deal_player, Game, Dealt),
      { setup_deal(DealtGame#fgame{hit_count=DealtGame#fgame.hit_count + 1, deck=Remain}),
        [{card_move, deck, {Target#fplayer.id, Stack#fstack.id}, Card}] };
    %This case covers the ground
    hits ->
      Target=get_player(Game#fgame.deal_player, Game#fgame.players),
      { NextGame, Deltas } = case Target#fplayer.hand of
        stop ->
          DealtPlayer = Target#fplayer{hand=none},
          DealtGame=set_player(Game#fgame.deal_player, Game, DealtPlayer),
          { setup_deal(DealtGame#fgame{hit_count=DealtGame#fgame.hit_count + 1}),
            [{card_move, deck, trash, ground},{card_move, {Target#fplayer.id, hand}, trash, stop}] };
        _OtherCard ->
          DeadPlayer = Target#fplayer{dead=true},
          DealtGame=set_player(Game#fgame.deal_player, Game, DeadPlayer),
          { setup_deal_player(DealtGame),
            [{card_move, deck, {Target#fplayer.id, Game#fgame.deal_stack}, ground}] }
      end,
      case get_winner(NextGame) of
        none ->
          { NextGame, Deltas };
        Winner ->
          {game_over, Winner, Deltas, NextGame }
      end
  end.

get_winner(Game) ->
  get_winner(Game#fgame.players, none).

get_winner([], LivePlayer) ->
  LivePlayer;
get_winner([Player | Players], none) ->
  case Player#fplayer.dead of
    true ->
      get_winner(Players, none);
    false ->
      get_winner(Players, Player#fplayer.id)
  end;
get_winner([Player | Players], LivePlayer) ->
  case Player#fplayer.dead of
    true ->
      get_winner(Players, LivePlayer);
    false ->
      none
  end.
    
  

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
  Player = get_player(Game#fgame.deal_player, Game#fgame.players),
  Clean = cleanup_stacks(Player),
  { RiderHandled, NextPlayer, Moved } = case Player#fplayer.rider of
    none ->
      {Game, Clean, false};
    hit ->
      {Game#fgame{hit_max = 2 + Player#fplayer.extras},
       Clean#fplayer{rider=none, extras=0},true};
    skip when length(Game#fgame.deck) == 0 ->
      {Game, Clean#fplayer{rider=none, extras=0}, true};
    skip when Clean#fplayer.extras > 0->
      {setup_deal_player(Game), Clean#fplayer{extras=Clean#fplayer.extras - 1}, extra};
    skip ->
      {setup_deal_player(Game), Clean#fplayer{rider=none}, true};
    split ->
      SplitPlayer = add_stacks(1+Player#fplayer.extras, Clean),
      {set_player(Player#fplayer.id, Game, SplitPlayer),
       Clean#fplayer{rider=none, extras=0},true}
  end,
  NextGame = set_player(Player#fplayer.id, RiderHandled, NextPlayer),
  DeltaList = case Moved of
    extra ->
      [ {card_move, {Player#fplayer.id, rider}, trash, extra } ];
    true ->
      [ {card_move, {Player#fplayer.id, rider}, trash, Player#fplayer.rider } ];
    false ->
      bad_move
  end,
  { NextGame#fgame{deal_stage=hits}, DeltaList }.
    

setup_deal(Game) ->
  case Game#fgame.hit_count of
    Hits when Hits < Game#fgame.hit_max ->
      Game#fgame{hit_count = Hits + 1};
    _MaxHits ->
      setup_deal_stack(Game)
  end.

next_player(CurPlayerNum, GamePlayers) ->
  NextPlayerNum = CurPlayerNum +1,
  WrappedNextPlayerNum = case GamePlayers of
    Players when length(Players) >= NextPlayerNum ->
      NextPlayerNum;
    _Players ->
      io:format("Next Stack: ~w~n", [NextPlayerNum]),
      io:format("Stacks length: ~w~n", [length(GamePlayers)]),
      1
  end,
  case get_player(WrappedNextPlayerNum, GamePlayers) of
    #fplayer{dead=true} ->
      next_player(WrappedNextPlayerNum, GamePlayers);
    #fplayer{dead=false} ->
      WrappedNextPlayerNum
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
  Target=get_player(Game#fgame.deal_player, Game#fgame.players),
  NextStack = next_stack(Game#fgame.deal_stack, Target),
  case NextStack of
    none ->
      setup_deal_player(Game);
    StackNum ->
      Game#fgame{deal_stack=StackNum, hit_count=0}
  end.

setup_deal_player(Game) ->
  NextPlayerNum = next_player(Game#fgame.deal_player, Game#fgame.players),
  NextPlayer = get_player(NextPlayerNum, Game#fgame.players),
  TopStack = hd(NextPlayer#fplayer.stacks),
  Game#fgame{deal_player=NextPlayerNum, hit_count=0, deal_stack=TopStack#fstack.id, deal_stage=rider}.

draw_card(PlayerId, StackId, Game) ->
  Player = get_player(PlayerId, Game#fgame.players),
  case Player#fplayer.hand of
    none ->
      Stack = get_stack(StackId, Player),
      case Stack#fstack.cards of
        [] ->
          {Game, bad_move};
        [Card | Remain] ->
          Stack#fstack.cards,
          NewStackPlayer = set_stack(StackId,Player, Stack#fstack{cards=Remain}),
          DrawPlayer = NewStackPlayer#fplayer{hand=Card},
          { set_player(PlayerId, Game, DrawPlayer),
            [{card_move, {PlayerId, StackId}, {PlayerId, hand}, Card}]}
      end;
    _SomeCard ->
      { Game, bad_move }
  end.
  
play_card(FromPlayerId, ToPlayerId, Game) ->
  FromPlayer = get_player(FromPlayerId, Game#fgame.players),
  case FromPlayer#fplayer.hand of
    none ->
      { Game, bad_move };
    Card when FromPlayerId == ToPlayerId ->
      {NextFromPlayer, Deltas} = do_card(Card, FromPlayer, on_self),
      {set_player(FromPlayerId, Game, NextFromPlayer), Deltas};
    Card ->
      ToPlayer = get_player(ToPlayerId, Game#fgame.players),
      {NextFromPlayer, NextToPlayer, Deltas} = do_card(Card, FromPlayer, ToPlayer),
      FromUpdate = set_player(FromPlayerId, Game, NextFromPlayer),
      {set_player(ToPlayerId, FromUpdate, NextToPlayer), Deltas}
    end.

do_card(grab, Player, on_self) ->
  { Player, bad_move };
do_card(grab, FromPlayer, ToPlayer) ->
  case FromPlayer#fplayer.rider of
    none ->
      case ToPlayer#fplayer.rider of
        none ->
          {FromPlayer, ToPlayer,bad_move};
        Rider ->
          { FromPlayer#fplayer{hand=none, rider=Rider},
            ToPlayer#fplayer{rider=none},
            [{card_move, {FromPlayer#fplayer.id, hand}, trash, grab},
             {card_move, {ToPlayer#fplayer.id, rider}, {FromPlayer#fplayer.id, rider}, Rider}]}
      end;
    _Rider ->
      {FromPlayer, ToPlayer, bad_move}
  end;
do_card(push, Player, on_self) ->
  { Player, bad_move };
do_card(push, FromPlayer, ToPlayer) ->
  case ToPlayer#fplayer.rider of
    none ->
      case FromPlayer#fplayer.rider of
        none ->
          { FromPlayer, ToPlayer, bad_move };
        Rider ->
          { FromPlayer#fplayer{hand=none, rider=none},
            ToPlayer#fplayer{rider=Rider},
            [{card_move, {FromPlayer#fplayer.id, hand}, trash, push},
             {card_move, {FromPlayer#fplayer.id, rider}, {ToPlayer#fplayer.id, rider}, Rider}]}
      end;
    _Rider ->
      {FromPlayer, ToPlayer, bad_move}
  end;
do_card(stop, Player, on_self) ->
  case Player#fplayer.rider of
    none ->
      {Player, bad_move};
    Rider ->
      { Player#fplayer{hand=none, rider=none},
        [{card_move, {Player#fplayer.id, hand}, trash, push},
         {card_move, {Player#fplayer.id, rider}, trash, Rider}] }
  end;
do_card(stop, FromPlayer, ToPlayer) ->
  case FromPlayer#fplayer.rider of
    none ->
      {FromPlayer, ToPlayer, bad_move};
    Rider ->
      {FromPlayer#fplayer{hand=none}, ToPlayer#fplayer{rider=none},
        [{card_move, {FromPlayer#fplayer.id, hand}, trash, push},
         {card_move, {ToPlayer#fplayer.id, rider}, trash, Rider}] }
  end;
do_card(extra, Player, on_self) ->
  case Player#fplayer.rider of
    none ->
      {Player,bad_move};
    _Rider ->
      { Player#fplayer{extras=Player#fplayer.extras + 1},
        [{card_move, {Player#fplayer.id, hand}, {Player#fplayer.id, rider}, extra}]}
  end;
do_card(extra, FromPlayer, ToPlayer) ->
  case ToPlayer#fplayer.rider of
    none ->
      {FromPlayer, ToPlayer};
    _Rider ->
      {FromPlayer#fplayer{hand=none}, ToPlayer#fplayer{extras=ToPlayer#fplayer.extras + 1},
        [{card_move, {FromPlayer#fplayer.id, hand}, {ToPlayer#fplayer.id, rider}, extra}]}
  end;
do_card(Rider, Player, on_self) ->
  case Player#fplayer.rider of
    none ->
      { Player#fplayer{hand = none, rider = Rider},
        [{card_move, {Player#fplayer.id, hand}, {Player#fplayer.id, rider}, Rider}]};
    _Rider ->
      { Player, bad_move}
  end;
do_card(Rider, FromPlayer, ToPlayer) ->
  case ToPlayer#fplayer.rider of
    none ->
      {FromPlayer#fplayer{hand=none}, ToPlayer#fplayer{rider=Rider},
        [{card_move, {FromPlayer#fplayer.id, hand}, {ToPlayer#fplayer.id, rider}, Rider}]};
    _Rider ->
      {FromPlayer, ToPlayer, bad_move}
  end.

