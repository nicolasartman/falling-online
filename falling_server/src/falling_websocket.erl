-module(falling_websocket).
-include("include/game_meta.hrl").
-behavior(cowboy_http_handler).
-behavior(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
  io:format("Initializing http handler~n"),
  case cowboy_http_req:header('Upgrade', Req) of
    {undefined, Output} -> {ok, Output, undefined};
    {<<"websocket">>, _Output} -> {upgrade, protocol, cowboy_http_websocket};
    {<<"WebSocket">>, _Output} -> {upgrade, protocol, cowboy_http_websocket}
  end.

handle(Req, State) ->
  io:format("Handling request~n"),
  Pid = falling_instance:new_instance(),
  GameId = erlang:phash2(Pid),
  register_game(GameId, Pid),
  io:format("Created game ~w~n", [GameId]),
  JsonReply = falling_json:create_new_game_response(GameId),
  {ok, Output} = cowboy_http_req:reply(200, [], JsonReply, Req),
  {ok, Output, State}.

register_game(GameId, Pid) ->
  ets:insert(falling_instances, #falling_mapping{game_id=GameId, pid=Pid}).

terminate(_Req, _State) ->
  ok.

websocket_init(_Any, Req, []) ->
  CompactedReq = cowboy_http_req:compact(Req),
  %Pid = Req#http_req.pid,
  %{reply, {text, create_error("No game found")}, Output, undefined}.
  {ok, CompactedReq, undefined, hibernate}.

websocket_handle({text, Msg}, Req, PlayerId) ->
  {reply, {text, falling_json:create_error("No game found", "game_not_found")}, Req, PlayerId}.

websocket_info(JsonMsg, Req, PlayerId) ->
  %io:format("Sending message ~s to ~w~n", [JsonMsg, self()]),
  {reply, {text, JsonMsg}, Req, PlayerId}.

websocket_terminate(Reason, Req, PlayerId) ->
  ok.
