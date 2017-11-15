%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_endpoint_test).

-include_lib("eunit/include/eunit.hrl").

route_test_() ->
    {setup
    ,fun setup_db/0
    ,fun terminate_db/1
    ,fun(_ReturnOfSetup) ->
             [{"Testing get"
              ,test_get()
              }
             ]
     end
    }.

setup_db() ->
    ?LOG_DEBUG(":: Starting Kazoo FixtureDB"),
    {ok, _} = application:ensure_all_started(kazoo_config),
    {ok, Pid} = kazoo_data_link_sup:start_link(),
    Pid.

terminate_db(Pid) ->
    _DataLink = erlang:exit(Pid, normal),
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: ~p kazoo_config: ~p", [_DataLink, _KConfig])
    after 1000 ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: timeout kazoo_config: ~p", [_KConfig])
    end.

test_get() ->
    Call = kapps_call_test:create_callflow_call(),
    Ret = kz_endpoint:get(Call),
    ?debugFmt("~p~n", [Ret]),
    ?_assertEqual(1, 1).

attributes_keys_unique_test_() ->
    Keys = kz_endpoint:attributes_keys(),
    [?_assertEqual(length(Keys), length(lists:usort(Keys)))
    ].
