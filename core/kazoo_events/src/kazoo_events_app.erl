%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kazoo_events_app).
-behaviour(application).

-include("kazoo_events.hrl").

-export([start/2, stop/1]).
-export([start/0]).


-spec start() -> {'ok', atoms()}.
start() ->
    {'ok', _Apps} = application:ensure_all_started(?APP).

%% Application callbacks

%% @public
%% @doc Implement the application start behaviour
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_events_sup:start_link().

%% @public
%% @doc Implement the application stop behaviour
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
