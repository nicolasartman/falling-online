-module(falling_json).
-export([deltas_to_json/1]).

create_message_form(MessageType, Data) ->
  {[ {<<"messageType">>, MessageType}, {<<"data">>, Data} ]}.

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
