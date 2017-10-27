-module(teletype_publish).

-include_lib("eunit/include/eunit.hrl").
-include("teletype.hrl").

-define(TEST_POOL_ARGS, [{worker_module, teletype_renderer}
                        ,{name, {local, teletype_render_farm}}
                        ,{size, 1}
                        ,{max_overflow, 1}
                        ]).

-spec teletype_publish_test_() -> any().
teletype_publish_test_() ->
    {setup
    ,fun setup_test/0
    ,fun teardown/1
    ,fun(_ReturnOfSetup) ->
             [{"Test publishing " ++ binary_to_list(Id) ++ " notification"
              ,pulish_notification(Id, PubFun)
              }
              || {Id, PubFun} <- [{teletype_voicemail_to_email:id(), fun kapi_notifications:publish_voicemail_new/1}
                                 % ,{teletype_deregister:id(), fun kapi_notifications:publish_deregister/1}
                                 ]
             ]
     end
    }.

setup_test() ->
    ?LOG_DEBUG(":: Setting up Teletype publish test"),
    mock_them_all(),

    {ok, _} = application:ensure_all_started(kazoo_bindings),
    {ok, _} = application:ensure_all_started(kazoo_config),
    {ok, LinkPid} = kazoo_data_link_sup:start_link(),

    lager:set_loglevel(lager_console_backend, none),
    lager:set_loglevel(lager_file_backend, none),
    lager:set_loglevel(lager_syslog_backend, none),

    ok = application:ensure_started(poolboy),
    {ok, BoyPid} = poolboy:start_link(?TEST_POOL_ARGS),
    ignore = teletype_bindings:start_link(),

    {LinkPid, BoyPid}.

teardown({LinkPid, BoyPid}) ->
    validate_mock(),
    stop_data_link(LinkPid),
    ok = poolboy:stop(BoyPid),
    _ = [application:stop(App) || App <- [kazoo_bindings, kazoo_etsmgr, lager, poolboy]],
    meck:unload().

stop_data_link(LinkPid) ->
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

pulish_notification(TemplateId, PubFun) ->
    TemplateIdStr = binary_to_list(TemplateId),
    Path = "notif__" ++ TemplateIdStr ++ ".json",
    {ok, Payload} = kz_json:fixture(?APP, Path),
    ?_assertEqual(<<"completed">>, get_status(kapps_notify_publisher:call_collect(Payload, PubFun))).

get_status([]) -> 'false';
get_status([JObj|_]) ->
    kz_json:get_ne_binary_value(<<"Status">>, JObj);
get_status({_, JObj}) ->
    get_status([JObj]).

mock_them_all() ->
    meck:new(kz_amqp_worker, [unstick, passthrough]),
    meck:expect(kz_amqp_worker, call_collect, kz_amqp_worker_call_collect()),
    meck:expect(kz_amqp_worker, cast, kz_amqp_worker_cast()),

    meck:new(amqp_util, [unstick, passthrough]),
    meck:expect(amqp_util, targeted_publish, amqp_util_targeted_publish()),
    meck:expect(amqp_util, notifications_publish, amqp_util_notifications_publish()),

    meck:new(gen_smtp_client, [unstick, passthrough]),
    meck:expect(gen_smtp_client, send, smtp_send()).

validate_mock() ->
    meck:validate(kz_amqp_worker),
    meck:validate(amqp_util),
    meck:validate(gen_smtp_client).

kz_amqp_worker_call_collect() ->
    fun(Req, PublishFun, _, Timeout) ->
            ?LOG_DEBUG("publishing notification"),
            PublishFun(Req),
            receive
                {ok, _}=OK -> OK;
                {error, _}=Error -> Error
            after Timeout ->
                    ?LOG_DEBUG("timeout"),
                    {timeout, kz_json:new()}
            end
    end.

kz_amqp_worker_cast() ->
    fun(Prop, PublishFun) ->
            PublishFun(Prop)
    end.

amqp_util_targeted_publish() ->
    fun(_, Payload, _) ->
            ?LOG_DEBUG("publishing teletype response"),
            self() ! {ok, kz_json:decode(Payload)}
    end.

amqp_util_notifications_publish() ->
    fun(_, Payload, _) ->
            teletype_bindings:notification(kz_json:decode(Payload))
    end.

smtp_send() ->
    fun({From, To, _}, _, CallBack) ->
            ?LOG_DEBUG("simulating sending from ~p to ~p", [From, To]),
            CallBack({ok, <<"Message accepted">>})
    end.
