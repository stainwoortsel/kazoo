%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_fixtures_util).

%% Driver callbacks
-export([format_error/1
        ]).

-include("kz_fixtures.hrl").

%%%===================================================================
%%% Driver callbacks
%%%===================================================================

-spec format_error(any()) -> any().
format_error('timeout') -> 'timeout';
format_error('conflict') -> 'conflict';
format_error('not_found') -> 'not_found';
format_error('db_not_found') -> 'db_not_found'.
