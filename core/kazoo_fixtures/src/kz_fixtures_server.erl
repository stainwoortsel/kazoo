%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_fixtures_server).

%% Driver callbacks
-export([new_connection/1
        ]).

%% Server callbacks
-export([server_info/1
        ,server_url/1
        ,get_db/2
        ,db_url/2
        ]).

%% API
-export([get_app_connection/1
        ,maybe_use_app_connection/2
        ]).

-include("kz_fixtures.hrl").

%%%===================================================================
%%% Driver callbacks
%%%===================================================================

-spec new_connection(map()) -> kz_data:connection().
new_connection(Map) ->
    Url = code:priv_dir(kazoo_fixtures),
    {'ok', #{url => Url ++ "/fixture_dbs"
            ,options => Map
            }
    }.

%%%===================================================================
%%% Connection operations
%%%===================================================================

-spec get_db(kz_data:connection(), ne_binary()) -> map().
get_db(Server, DbName) ->
    ConnToUse = maybe_use_app_connection(Server, DbName),
    #{server => ConnToUse, name => DbName}.

-spec server_url(kz_data:connection()) -> ne_binary().
server_url(#{url := Url}) ->
    Url.

-spec db_url(kz_data:connection(), ne_binary()) -> ne_binary().
db_url(Server, DbName) ->
    #{url := Url} = maybe_use_app_connection(Server, DbName),
    <<(iolist_to_binary(Url))/binary, "/", DbName/binary>>.


-spec server_info(kz_data:connection()) -> doc_resp().
server_info(_Server) ->
    {'ok', kz_json:from_list(
             [{<<"kazoo">>, <<"Willkommen">>}
             ,{<<"version">>, <<"0.0.0.0.0.0.0.1α">>}
             ,{<<"features">>, [<<"cool">>]}
             ,{<<"vendor">>, kz_json:from_list([{<<"name">>, <<"the Great FixtureDB Committee">>}])}
             ])
    }.

%%%===================================================================
%%% API
%%%===================================================================

-spec get_app_connection(kz_data:connection()) -> kz_data:connection().
get_app_connection(#{options := Options}=Server) ->
    case maps:get(test_app, Options, 'unedfined') of
        'unedfined' -> Server;
        AppName ->
            set_app_connection(Server, AppName)
    end.

-spec maybe_use_app_connection(kz_data:connection(), ne_binary()) -> kz_data:connection().
maybe_use_app_connection(#{options := Options}=Server, DbName) ->
    case {maps:get(test_app, Options, 'unedfined')
         ,maps:get(test_db, Options, 'unedfined')
         }
    of
        {'unedfined', _} -> Server;
        {_AppName, 'unedfined'} ->
            ?LOG_DEBUG("test_db is not set, using kazoo_fixtures database..."),
            Server;
        {AppName, DbName} ->
            set_app_connection(Server, AppName);
        {_AppName, _OtherDb} ->
            ?LOG_DEBUG("requested db ~s is not test_db ~s, using kazoo_fixtures database path...", [DbName, _OtherDb]),
            Server
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec set_app_connection(kz_data:connection(), atom()) -> kz_data:connection().
set_app_connection(#{options := Options}=Server, AppName) ->
    Path = case maps:get(test_db_subdir, Options, 'unedfined') of
               'unedfined' -> code:priv_dir(AppName);
               P -> code:lib_dir(AppName, kz_term:to_atom(P, 'true'))
           end,
    case Path of
        {'error', 'bad_name'} ->
            ?LOG_DEBUG("bad_name for path ~p, using default kazoo_fixtures database path...", [AppName]),
            Server;
        _ ->
            Server#{url => Path}
    end.
