%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_fixturedb_app).

-behaviour(application).

-include("kz_fixturedb.hrl").

%% Application callbacks
-export([start/0, start/2, stop/1]).

-spec start() -> {'ok', pid()}.
start() ->
    {ok, _} -> application:ensure_all_started(kazoo_config),
    kazoo_data_link_sup:start_link().

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_fixturedb_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
