%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_fixtures_db).

%% DB operations
-export([db_create/3
        ,db_delete/2
        ,db_view_cleanup/2
        ,db_info/1, db_info/2
        ,db_exists/2
        ,db_archive/3
        ,db_list/2
        ]).

-include("kz_fixtures.hrl").

%%%===================================================================
%%% DB operations
%%%===================================================================

-spec db_create(kz_data:connection(), ne_binary(), kz_data:options()) -> boolean().
db_create(_Server, _DbName, _Options) ->
    'true'.

-spec db_delete(kz_data:connection(), ne_binary()) -> boolean().
db_delete(_Server, _DbName) ->
    'true'.

-spec db_view_cleanup(kz_data:connection(), ne_binary()) -> boolean().
db_view_cleanup(_Server, _DbName) ->
    'true'.

-spec db_info(kz_data:connection()) -> {'ok', ne_binaries()} | fixture_error().
db_info(#{url := ServerUrl}=Server) ->
    #{url := AppUrl} = kz_fixtures_server:get_app_connection(Server),
    get_dbs_list(ServerUrl, AppUrl).


-spec db_info(kz_data:connection(), ne_binary()) -> docs_resp().
db_info(Server, DbName) ->
    case db_exists(Server, DbName) of
    	'false' -> {'error', 'db_not_found'};
    	'true' -> {'ok', kz_json:from_list([{<<"db_name">>, DbName}])}
    end.

-spec db_exists(kz_data:connection(), ne_binary()) -> boolean().
db_exists(Server, DbName) ->
    #{url := Url} = kz_fixtures_server:maybe_use_app_connection(Server, DbName),
    filelib:is_dir(kz_term:to_list(Url)).

-spec db_archive(kz_data:connection(), ne_binary(), ne_binary()) -> 'ok'.
db_archive(_, _, _) ->
    'ok'.

-spec db_list(kz_data:connection(), kz_data:options()) -> docs_resp().
db_list(Server, _Options) ->
    db_info(Server).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_dbs_list(ne_binary(), ne_binary()) -> ne_binaries().
get_dbs_list(ServerUrl, ServerUrl) ->
	[kz_term:to_binary(Db) || Db <- get_dbs_list(ServerUrl)];
get_dbs_list(ServerUrl, AppUrl) ->
	lists:usort([kz_term:to_binary(Db)
				 || Db <- get_dbs_list(ServerUrl) ++ get_dbs_list(AppUrl)
				]).

-spec get_dbs_list(ne_binary()) -> ne_binaries().
get_dbs_list(Url) ->
	filelib:wildcard(kz_term:to_list(Url) ++ "/*").
