-module(falling_json).
-export([deltas_to_json/1, create_error/2, create_new_game_response/1, parse_client_message/1]).

parse_decoded_json({[ {<<"messageType">>, Type}, {<<"data">>, Data} ]}) ->
  parse_decoded_json(Type, Data).

parse_decoded_json(<<"connect">>, {[ {<<"gameID">>, GameId} ]}) ->
  {connect, GameId};
parse_decoded_json(<<"join">>, {Params}) ->
  {join, parse_param_list(Params, [])};
parse_decoded_json(<<"set_ready">>, {[Param]}) ->
  {set_ready, parse_param(Param)};
parse_decoded_json(<<"play_card">>, {[Param]}) ->
  {play_card, parse_param(Param)};
parse_decoded_json(<<"draw">>, {[Param]}) ->
  {draw, parse_param(Param)};
parse_decoded_json(<<"get_game_state">>, {[Param]}) ->
  {get_game_state, parse_param(Param)};
parse_decoded_json(<<"rematch">>, {[Param]}) ->
  {rematch, parse_param(Param)}.

parse_param_list([], ParsedParams) ->
  ParsedParams;
parse_param_list([Param | Params], ParsedParams) ->
  parse_param_list(Params, [parse_param(Param) | ParsedParams]).

parse_param({<<"gameID">>, GameId}) ->
  {game_id, GameId};
parse_param({<<"nickname">>, Nickname}) ->
  {nickname, Nickname};
parse_param({<<"playerType">>, PlayerType}) ->
  case PlayerType of
    <<"player">> ->
      {player_type, player};
    <<"spectator">> ->
      {player_type, spectator}
  end;
parse_param({<<"ready">>, Ready}) ->
  {ready, Ready};
parse_param({<<"target">>, Target}) when is_integer(Target)->
  {target, Target}.

encode_reply({connected, Started, PlayerList}) ->
  create_message(<<"connected">>, {[ {<<"started">>, Started]}, {<<"players">>, PlayerList} ]});
encode_reply({error, Reason, Desc}) ->
  create_message(<<"error">>, {[ {<<"errorType">>, reason_to_text(Reason)]}, {<<"errorMessage">>, Desc} ]}).

reason_to_text(already_started) ->
  <<"already_started">>.

parse_client_message(Message) ->
  Decoded = jiffy:decode(Message),
  parse_decoded_json(Decoded).

create_message(MessageType, Data) ->
  jiffy:encode(create_message_form(MessageType, Data)).

create_message_form(MessageType, Data) ->
  {[ {<<"messageType">>, MessageType}, {<<"data">>, Data} ]}.

create_new_game_response(GameId) ->
  create_message(<<"game_created">>, {[ { <<"gameID">>, GameId} ]}).

create_error(Message, MsgType) ->
  create_message(<<"error">>, {[ {<<"errorMessage">>, Message}, {<<"errorType">>, MsgType}]}).

deltas_to_json(Deltas) ->
  JsonForm = create_deltas(Deltas, []),
  jiffy:encode(JsonForm).

create_deltas([], Message) ->
  lists:reverse(Message);
create_deltas([{card_move, Source, Dest, Card} | Deltas], MessageList) ->
  SourceForm = create_location(Source),
  DestForm = create_location(Dest),
  MessageForm = create_message_form(<<"card_move">>, 
                       {[ {<<"from">>, SourceForm},
                       {<<"to">>,  DestForm},
                       {<<"card">>, encode_card(Card)} ]}),
  create_deltas(Deltas, [MessageForm | MessageList]).

create_location({PlayerId, Location}) ->
  LocationForm = case Location of 
    hand -> <<"hand">>;
    rider -> <<"rider">>;
    StackId -> StackId
  end,
    
  {[ {<<"playerId">>, PlayerId}, {<<"location">>, LocationForm}]};
create_location(deck) ->
  <<"deck">>;
create_location(trash) ->
  <<"trash">>.

encode_card(Card) ->
  case Card of
    hit -> <<"hit">>;
    split -> <<"split">>;
    extra -> <<"extra">>;
    stop -> <<"stop">>;
    grab -> <<"grab">>;
    push -> <<"push">>
  end.
