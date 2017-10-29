%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kapps_account_config_test).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_config.hrl").

-define(MASTER_ACCOUNT_ID, <<"account0000000000000000000000001">>).
-define(RESELLER_ACCOUNT_ID, <<"account0000000000000000000000002">>).
-define(PARENT_ACCOUNT_ID, <<"account0000000000000000000000003">>).
-define(SUB_ACCOUNT_ID, <<"account0000000000000000000000004">>).

-define(SUB_PARENT_RESELLER_SYSTEM, <<"test_account_config">>).
-define(RESELLER_EMPTY, <<"test_account_config_reseller_empty">>).
-define(RESELLER_ONLY, <<"test_account_config_reseller_only">>).
-define(SYSTEM_EMPTY, <<"test_account_config_system_empty">>).
-define(SYSTEM_ONLY, <<"test_account_config_system_only">>).

kz_account_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
    ,fun(Map) ->
             [test_get_ne_binary(Map)
             ,test_get(Map)
             ,test_get_global(Map)
             ,test_get_from_reseller(Map)
             ,test_get_with_strategy(Map)
             ]
     end
    }.

setup() ->
    ?LOG_DEBUG(":: Setting up Kazoo FixtureDB"),

    {ok, _} = application:ensure_all_started(kazoo_config),
    {ok, LinkPid} = kazoo_data_link_sup:start_link(),

    #{pid => LinkPid
     ,sub_config => get_fixture(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM)
     ,reseller_only_config => get_fixture(?RESELLER_ACCOUNT_ID, ?RESELLER_ONLY),
     ,system_config => get_fixture_value(<<"default">>, ?KZ_CONFIG_DB, ?SUB_PARENT_RESELLER_SYSTEM)
     }.

cleanup(#{ pid := LinkPid}) ->
    _DataLink = erlang:exit(LinkPid, normal),
    Ref = monitor(process, LinkPid),
    receive
        {'DOWN', Ref, process, LinkPid, _Reason} ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: ~p kazoo_config: ~p", [_DataLink, _KConfig])
    after 1000 ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: timeout kazoo_config: ~p", [_KConfig])
    end.

test_get_ne_binary(_) ->
    [{"Testing get_ne_binary account config"
     ,[{"get a key with binary value"
       ,?_assertEqual(true, is_ne_binary(kapps_account_config:get_ne_binary(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, [<<"root_obj_key">>, <<"b_key">>])))
       }
      ,{"get a non cast-able to binary value should crash"
       ,?_assertThrow(badarg, kapps_account_config:get_ne_binary(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, [<<"root_obj_key">>, <<"obj_key">>]))
       }
      ,{"get a non-empty value which is cast-able to binary should return it as a non-empty binary"
       ,?_assertEqual(true, is_ne_binary(kapps_account_config:get_ne_binary(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, [<<"root_obj_key">>, <<"i_key">>])))
       }
      ,{"get an empty value should return default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binary(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, <<"not_exists">>))
       }
      ,{"get a list of binary value should return list of binary"
       ,?_assertEqual(true, is_ne_binaries(kapps_account_config:get_ne_binaries(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, [<<"root_obj_key">>, <<"b_keys">>])))
       }
      ,{"get not a list of binary value should return Default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binaries(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, [<<"root_obj_key">>, <<"b_key">>]))
       }
      ,{"get an empty list of binary value should return Default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binaries(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, [<<"root_obj_key">>, <<"b_key">>]))
       }
      ]
     }
    ].

is_ne_binary(Value) -> kz_term:is_ne_binary(Value).
is_ne_binaries(Value) -> kz_term:is_ne_binaries(Value).

test_get(_) ->
    [{"Testing account get config"
     ,[{"customized account should result in account"
       ,?_assertEqual(<<"sub_account">>, kapps_account_config:get(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, [<<"root_obj_key">>, <<"b_key">>]))
       }
      ,{"not customized property in account should result in Default"
       ,?_assertEqual(<<"me_don_you">>, kapps_account_config:get(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM, <<"udon_me">>, <<"me_don_you">>))
       }
      ]
     }
    ].

test_get_global(#{sub_config := SubAccountConfig
                 ,reseller_only_config := ResellerOnly
                 ,system_config := SystemConfig
                 }=Map) ->
    FunToTest = fun(AccountId, Category, Key) ->
                        kapps_account_config:get_global(AccountId, Category, Key)
                end,
    [{"Testing get global account config"
     ,[{"customized sub-account on get_global/2 should result in account"
       ,?_assertEqual(SubAccountConfig, kapps_account_config:get_global(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM))
       }
      ,{"undefined account_id on get_global/2 should result in system_config"
       ,?_assertEqual(SystemConfig, kapps_account_config:get_global(undefined, ?SUB_PARENT_RESELLER_SYSTEM))
       }
      ,{"not customized sub-account and customized reseller on get_global/2 should result in reseller"
       ,?_assertEqual(ResellerOnly, kapps_account_config:get_global(?SUB_ACCOUNT_ID, ?RESELLER_ONLY))
       }
      ,{"not customized sub-account and reseller on get_global/2 should result in system_config"
       ,?_assertEqual(SystemConfig, kapps_account_config:get_global(?SUB_ACCOUNT_ID, ?SUB_PARENT_RESELLER_SYSTEM))
       }
      ,{"not customized sub-account and reseller and empty system_config on get_global/2 should result in empty"
       ,?_assertEqual(kz_doc:set_id(kz_json:new(), ?SYSTEM_EMPTY), kapps_account_config:get_global(?PARENT_ACCOUNT_ID, ?SYSTEM_EMPTY))
       }
      ,{"non existing category on get_global/2 should result in empty"
       ,?_assertEqual(kz_doc:set_id(kz_json:new(), <<"no_cat_please">>), kapps_account_config:get_global(?PARENT_ACCOUNT_ID, <<"no_cat_please">>))
       }
      ,common_tests_for_get_global(FunToTest, Map)
      ]
     }
    ].

common_tests_for_get_global(Fun, #{}) ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], ?KZ_CONFIG_DB, ?SYSTEM_ONLY),
    ResellerValue = get_fixture_value(<<"root_obj_key">>, ?RESELLER_ACCOUNT_ID, ?RESELLER_CUSTOMIZED),
    AccountValue = get_fixture_value(<<"root_obj_key">>, ?PARENT_ACCOUNT_ID, ?PARENT_ONLY),

    [{"Common get global account config"
     ,[{"undefined account id should result in system_config"
       ,?_assertEqual(SysValue, Fun(undefined, ?SUB_PARENT_RESELLER_SYSTEM, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and reseller should result in system_config"
       ,?_assertEqual(<<"system_only">>, Fun(?SUB_ACCOUNT_ID, ?SYSTEM_ONLY, <<"key">>))
       }
      ,{"not customized sub-account and customized reseller should result in reseller"
       ,?_assertEqual(ResellerValue, Fun(?PARENT_ACCOUNT_ID, ?RESELLER_CUSTOMIZED, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and empty customized reseller should result in system_config"
       ,?_assertEqual(<<"system_only">>, Fun(?PARENT_ACCOUNT_ID, ?RESELLER_EMPTY, [<<"root_obj_key">>, <<"system_key">>]))
       }
      % ,{"empty customized sub-account and customized reseller should result in reseller"       <---- look at me
      %  ,?_assertEqual(SysValue, Fun(?PARENT_ACCOUNT_ID, ?RESELLER_CUSTOMIZED, <<"root_obj_key">>))
      %  }
       ,{"empty customized sub-account should result in system_config"
       ,?_assertEqual(SysValue, Fun(?PARENT_ACCOUNT_ID, ?SUB_SUB_EMPTY, <<"root_obj_key">>))
       }
      ,{"customized sub-account should result in account"
       ,?_assertEqual(AccountValue, Fun(?PARENT_ACCOUNT_ID, ?PARENT_ONLY, <<"root_obj_key">>))
       }
      ]
     }
    ].

test_get_from_reseller() ->
    FunToTest = fun(Args) when length(Args) =:= 3 ->
                        apply(fun kapps_account_config:get_from_reseller/3, Args);
                   (Args) when length(Args) =:= 4 ->
                        apply(fun kapps_account_config:get_from_reseller/4, Args)
                end,
    [{"Testing get config from reseller"
     ,common_tests_for_get_from_reseller(FunToTest)
     }
    ].

common_tests_for_get_from_reseller(FunToTest) ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], ?KZ_CONFIG_DB, ?SYSTEM_ONLY),
    ResellerValue = get_fixture_value(<<"root_obj_key">>, ?RESELLER_ACCOUNT_ID, ?RESELLER_CUSTOMIZED),

    [{"Common get config from reseller tests"
     ,[{"undefined account id should result in system_config"
       ,?_assertEqual(SysValue, FunToTest([undefined, ?SYSTEM_ONLY, <<"root_obj_key">>]))
       }
      ,{"not customized reseller should result in system_config"
       ,?_assertEqual(SysValue, FunToTest([?PARENT_ACCOUNT_ID, ?SYSTEM_ONLY, <<"root_obj_key">>]))
       }
      ,{"not customized reseller and empty system_config should result in set Default on system_config"
       ,?_assertEqual(Default, FunToTest([?PARENT_ACCOUNT_ID, ?SYSTEM_ONLY, <<"new_key">>, <<"default">>]))
       }
      ,{"empty customized reseller should result in system_config"
       ,?_assertEqual(SysValue, FunToTest([?PARENT_ACCOUNT_ID, ?RESELLER_EMPTY, <<"root_obj_key">>, Default]))
       }
      ,{"customized reseller should result in reseller"
       ,?_assertEqual(ResellerValue, FunToTest([?PARENT_ACCOUNT_ID, ?RESELLER_CUSTOMIZED, <<"root_obj_key">>, Default]))
       }
      ,{"only get from direct reseller"
       ,?_assertEqual(SysValue, FunToTest([?RESELLER_ACCOUNT_ID, ?RESELLER_CUSTOMIZED, <<"root_obj_key">>, Default]))
       }
      ]
     }
    ].

test_get_with_strategy() ->
    [{"Testing getting account config with strategy"
     ,[get_with_strategy_general()
      ,get_startegy_global()
      ,get_startegy_reseller()
      ,get_startegy_hierarchy_merge()
      ]
     }
    ].

get_with_strategy_general() ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], ?KZ_CONFIG_DB, ?SYSTEM_ONLY),

    Db = kz_util:format_account_db(?PARENT_ACCOUNT_ID),

    [{"Testing strategy with no account id"
     ,[{"undefined account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, undefined, ?SYSTEM_ONLY, <<"root_obj_key">>))
       }
      ,{"empty call object account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, kapps_call:new(), ?SYSTEM_ONLY, <<"root_obj_key">>))
       }
      ,{"empty jobj object account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, kz_json:new(), ?SYSTEM_ONLY, <<"root_obj_key">>))
       }
      ,{"unknown object account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, maps:new(), ?SYSTEM_ONLY, <<"root_obj_key">>))
       }
      ,{"passing non raw account id where account is not customized should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, Db, ?SYSTEM_ONLY, <<"root_obj_key">>))
       }
      ,{"passing non raw account id in jobj where account and reseller is not customized should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, kz_json:from_list([{<<"account_id">>, Db}]), ?SYSTEM_ONLY, <<"root_obj_key">>))
       }
      ]
     }
    ,{"Testing some general situation"
     ,[{"not customized account and reseller and empty system_config should result in set Default on system_config"
       ,?_assertEqual(<<"dummy_me">>, kapps_account_config:get_with_strategy(<<"global">>, ?PARENT_ACCOUNT_ID, ?PARENT_ONLY, <<"new_key">>, <<"dummy_me">>))
       }
      ,{"not customized account and reseller and not exists system_config should result in set Default on system_config"
       ,?_assertEqual(<<"dummy_me">>, kapps_account_config:get_with_strategy(<<"global">>, ?PARENT_ACCOUNT_ID, <<"no_cat_please">>, <<"new_key">>, <<"dummy_me">>))
       }
      ]
     }
    ].

get_startegy_global() ->
    FunToTest = fun(AccountId, Category, Key) ->
                        kapps_account_config:get_with_strategy(<<"global">>, AccountId, Category, Key)
                end,
    [{"Testing get config global strategy"
     ,common_tests_for_get_global(FunToTest)
     }
    ].

get_startegy_reseller() ->
    FunToTest = fun(Args) when length(Args) =:= 3 ->
                        apply(fun kapps_account_config:get_with_strategy/4, [<<"reseller">>|Args]);
                   (Args) when length(Args) =:= 4 ->
                        apply(fun kapps_account_config:get_with_strategy/5, [<<"reseller">>|Args])
                end,
    [{"Testing get config reseller strategy"
     ,common_tests_for_get_from_reseller(FunToTest)
     }
    ].

get_startegy_hierarchy_merge() ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], ?KZ_CONFIG_DB, ?SYSTEM_ONLY),
    SubAccount1Value = get_fixture_value(<<"root_obj_key">>, "test_cat_subaccount_1"),
    SubAccount2Value = get_fixture_value(<<"root_obj_key">>, "test_cat_subaccount_2"),
    ResellerValue = get_fixture_value(<<"root_obj_key">>, "test_cat_reseller"),

    SometimesEmptyValue_System = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], SysValue),
    SometimesEmptyValue_Reseller = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], ResellerValue),
    SometimesEmptyValue_Sub1 = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], SubAccount1Value),

    [{"Testing get config hierarchy_merge strategy"
     ,[{"customized account where account is reseller itself should result in merged value of reseller and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, ResellerValue])
                     ,kapps_account_config:get_with_strategy(<<"hierarchy_merge">>, ?RESELLER_ACCOUNT_ID, ?RESELLER_SYSTEM, <<"root_obj_key">>)
                     )
       }
      ,{"customized account 1 should result in merged value of account, reseller and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, ResellerValue, SubAccount1Value])
                     ,kapps_account_config:get_hierarchy(?PARENT_ACCOUNT_ID, ?SUB1_RESELLER_SYSTEM, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, system and empty/not_exists parent & reseller should result in merged value of account and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?SUB_ACCOUNT_ID, ?SUB2_SYSTEM, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, reseller, system and empty/not_exists parent should result in merged value of all customized and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, ResellerValue, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?SUB_ACCOUNT_ID, ?SUB2_RESELLER_SYSTEM, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, parent, system and empty reseller should result in merged value of all customized and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, SubAccount1Value, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?SUB_ACCOUNT_ID, ?SUB2_SUB1_EMPTYRESELLER_SYSTEM, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, parent, system and not customized reseller should result in merged value of all customized and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, SubAccount1Value, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?SUB_ACCOUNT_ID, ?SUB2_SUB1_SYSTEM, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, parents and system should result in merged value of all"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, ResellerValue, SubAccount1Value, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?SUB_ACCOUNT_ID, ?SUB2_SUB1_RESELLER_SYSTEM, <<"root_obj_key">>)
                     )
       }
      ,{"not customized account with undefined parent account id and no reseller should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_hierarchy(kz_binary:rand_hex(16), <<"a_cat">>, <<"root_obj_key">>))
       }
      ,{"check if the account set an empty jobj, the content of the hierarchy is merged into the jobj properly"
       ,?_assertEqual(kz_json:merge_recursive([SometimesEmptyValue_System, SometimesEmptyValue_Reseller, SometimesEmptyValue_Sub1])
                     ,kapps_account_config:get_hierarchy(?SUB_ACCOUNT_ID, ?SUB2_SUB1_RESELLER_SYSTEM, [<<"root_obj_key">>, <<"obj_empty_test">>, <<"obj_empty_sometimes">>])
                     )
       }
      ]
     }
    ].


get_fixture_value(Key, DbName, Category) ->
    {ok, JObj} = get_fixture(DbName, Category)
    kz_json:get_value(Key, JObj).

get_fixture(?KZ_CONFIG_DB, Category) ->
    Path = kz_fixturedb_util:get_doc_path(?KZ_CONFIG_DB, Category),
    kz_json:fixture(Path);
get_fixture(AccountId, Category) ->
    Path = kz_fixturedb_util:get_doc_path(kz_util:format_account_db(AccountId), Category),
    kz_json:fixture(Path).
