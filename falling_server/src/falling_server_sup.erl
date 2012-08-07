-module(falling_server_sup).
-include("include/game_meta.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  random:seed(now()),
  ets:new(falling_instances, [set, named_table, public, {keypos, #falling_mapping.game_id}]),
  %ets:new(falling_connections, [named_table, bag, public, {keypos, #falling_mapping.game_id}]),
  Dispatch = [ { '_', [ { [<<"play">>], falling_websocket, []} ,
                        { [<<"new_game">>], falling_websocket, []} ] } ],
  CowboySpec = cowboy:child_spec(falling_websocket, 2024,
                            cowboy_tcp_transport, [{port, 8081}],
                            cowboy_http_protocol, [{dispatch, Dispatch}]),
  {ok, { {one_for_one, 5, 10},
       [ CowboySpec ]}}.

