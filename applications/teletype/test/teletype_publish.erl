-module(hes_tests).

-include_lib("eunit/include/eunit.hrl").
-include("teletype.hrl").

-define(TEST_POOL_ARGS, [{worker_module, teletype_renderer}
                        ,{name, {local, teletype_render_farm}}
                        ,{size, 1}
                        ,{max_overflow, 1}
                        ]).

-spec publishing_test_() -> any().
publishing_test_() ->
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
    ?LOG_DEBUG("setting up test"),
    meck_all(),
    {ok, _} = application:ensure_all_started(kazoo_bindings),
    lager:set_loglevel(lager_console_backend, none),
    lager:set_loglevel(lager_file_backend, none),
    lager:set_loglevel(lager_syslog_backend, none),
    ok = application:ensure_started(poolboy),
    {ok, Pid} = poolboy:start_link(?TEST_POOL_ARGS),
    ignore = teletype_bindings:start_link(),
    Pid.

teardown(PoolBoyPid) ->
    ?LOG_DEBUG("tearing down test"),
    poolboy:stop(PoolBoyPid),
    _ = [application:stop(App) || App <- [kazoo_bindings, kazoo_etsmgr, lager, poolboy]],
    meck:unload().

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

meck_all() ->
    meck:new(kz_amqp_worker, [unstick, passthrough]),
    meck:expect(kz_amqp_worker, call_collect, kz_amqp_worker_call_collect()),
    meck:expect(kz_amqp_worker, cast, kz_amqp_worker_cast()),

    meck:new(amqp_util, [unstick, passthrough]),
    meck:expect(amqp_util, targeted_publish, amqp_util_targeted_publish()),
    meck:expect(amqp_util, notifications_publish, amqp_util_notifications_publish()),

    meck:new(kz_datamgr, [unstick, passthrough]),
    meck:expect(kz_datamgr, open_doc, open_doc_2()),
    meck:expect(kz_datamgr, open_doc, open_doc_3()),

    meck:expect(kz_datamgr, open_cache_doc, open_doc_2()),
    meck:expect(kz_datamgr, open_cache_doc, open_doc_3()),

    meck:expect(kz_datamgr, fetch_attachment, fetch_attachment_3()),
    meck:expect(kz_datamgr, put_attachment, put_attachment_4()),
    meck:expect(kz_datamgr, put_attachment, put_attachment_5()),

    meck:expect(kz_datamgr, get_results, get_results_2()),
    meck:expect(kz_datamgr, get_results, get_results_3()),

    meck:expect(kz_datamgr, save_doc, fun(_, JObj) -> {ok, JObj} end),
    meck:expect(kz_datamgr, save_doc, fun(_, JObj, _) -> {ok, JObj} end),

    meck:new(kz_cache, [unstick, passthrough]),
    meck:expect(kz_cache, store_local, store_cache()),

    % meck:new(teletype_util, [unstick, passthrough]),
    % meck:expect(teletype_util, send_update, teletype_util_send_update()),

    meck:new(gen_smtp_client, [unstick, passthrough]),
    meck:expect(gen_smtp_client, send, smtp_send()).

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

% teletype_util_send_update() -> fun(_, <<"pending">>) -> ok end.

store_cache() ->
    fun(?CACHE_NAME, {receipt, "Message accepted"}, _, _) -> ok;
       (_CacheName, _, _, _) ->
            ?LOG_DEBUG("cache for ~s not implemented", [_CacheName])
            % {_, ST} = erlang:process_info(self(), current_stacktrace),
            % kz_util:log_stacktrace(ST)
    end.

open_doc_2() -> fun(Db, Id) -> open_doc(Db, Id, []) end.
open_doc_3() -> fun(Db, Id, Options) -> open_doc(Db, Id, Options) end.

open_doc(Db, {_, Id}, Options) ->
    open_doc(Db, Id, Options);
open_doc(Db, Id, _Options) ->
    Path = <<"fixtures/", Db/binary, "__", Id/binary, ".json">>,
    case kz_json:fixture(?APP, Path) of
        {ok, _}=OK -> OK;
        {error, _} ->
            % ?LOG_DEBUG(" tried ~s/~s~n", [Db, Id]),
            % {_, ST} = erlang:process_info(self(), current_stacktrace),
            % kz_util:log_stacktrace(ST),
            {error, not_found}
    end.

fetch_attachment_3() ->
    fun(_Db, Id, <<"template.", _/binary>>=AttachmentName) ->
            Ext = ct_to_ext(AttachmentName),
            TemplateId = kz_notification:resp_id(Id),
            Path = filename:join([code:priv_dir(?APP), "templates", <<TemplateId/binary,".",Ext/binary>>]),
            file:read_file(Path);
       (_, _, _) ->
            {error, not_found}
    end.
ct_to_ext(<<"template.text/plain">>) -> <<"text">>;
ct_to_ext(<<"template.text/html">>) -> <<"html">>.

put_attachment_4() -> fun(Db, Id, AName, Content) -> put_attachment(Db, Id, AName, Content, []) end.
put_attachment_5() -> fun(Db, Id, AName, Content, Options) -> put_attachment(Db, Id, AName, Content, Options) end.

put_attachment(Db, Id, _AName, _Content, Options) ->
    case open_doc(Db, Id, Options) of
        {ok, _}=OK -> OK;
        {error, _}=Error -> Error
    end.

get_results_2() -> fun(Db, Id) -> get_results(Db, Id, []) end.
get_results_3() -> fun(Db, Id, Options) -> get_results(Db, Id, Options) end.

get_results(Db, DbView, _Options) ->
    AccountId = kz_util:format_account_id(Db),
    [View, Name] = binary:split(DbView, <<"/">>),
    Path = <<"get_results_fixtures/", AccountId/binary, "__", View/binary, "+", Name/binary, ".json">>,
    case kz_json:fixture(kazoo_documents, Path) of
        {ok, _}=OK -> OK;
        {error, _} ->
            % ?LOG_DEBUG(" tried ~s/~s~n", [Db, DbView]),
            % {_, ST} = erlang:process_info(self(), current_stacktrace),
            % kz_util:log_stacktrace(ST),
            {error, not_found}
    end.
