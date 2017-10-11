%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_notifications).

-export([init/0
        ,bindings_and_responders/0
        ,account_bindings/1
        ,handle_event/2
        ]).

-include("webhooks.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(NAME, <<"notifications">>).
-define(DESC, <<"Receive notifications as webhooks">>).

-define(NOTIFICATION_TYPES
       ,[kapi_notifications:definition_type(Definition) || Definition <- kapi_notifications:definitions()]
       ).

-define(TYPE_MODIFIER
       ,kz_json:from_list(
          [{<<"type">>, <<"array">>}
          ,{<<"description">>, <<"A list of notification types to handle">>}
          ,{<<"items">>, ?NOTIFICATION_TYPES}
          ])).

-define(MODIFIERS
       ,kz_json:from_list(
          [{<<"type">>, ?TYPE_MODIFIER}]
       )).

-define(METADATA
       ,kz_json:from_list(
          [{<<"_id">>, ?ID}
          ,{<<"name">>, ?NAME}
          ,{<<"description">>, ?DESC}
          ,{<<"modifiers">>, ?MODIFIERS}
          ])).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    Bindings = bindings(),
    Responders = [{{?MODULE, 'handle_event'}
                  ,[{<<"notification">>, <<"*">>}]
                  }
                 ],
    {Bindings, Responders}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_bindings(ne_binary()) -> gen_listener:bindings().
account_bindings(_AccountId) -> [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> any().
handle_event(JObj, _Props) ->
    lager:debug("got event: ~p", [JObj]),
    kz_util:put_callid(JObj),
    %% Need a way to validate the notification payload

    AccountId = kapi_notifications:account_id(JObj),
    case webhooks_util:find_webhooks(?NAME, AccountId) of
        [] ->
            lager:debug("no hooks to handle ~s for ~s"
                       ,[kz_api:event_name(JObj), AccountId]
                       );
        Hooks ->
            Event = format_event(JObj, AccountId),
            Type = kz_api:event_name(JObj),
            Filtered = [Hook || Hook <- Hooks, match_action_type(Hook, Type)],
            webhooks_util:fire_hooks(Event, Filtered)
    end.

-spec match_action_type(webhook(), api_binary()) -> boolean().
match_action_type(#webhook{hook_event = ?NAME
                          ,custom_data='undefined'
                          }, _Type) -> 'true';
match_action_type(#webhook{hook_event = ?NAME
                          ,custom_data=JObj
                          }, Type) ->
    kz_json:get_value(<<"type">>, JObj) =:= Type;
match_action_type(#webhook{}, _Type) -> 'true'.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bindings() -> gen_listener:bindings().
bindings() ->
    [{'notifications', [{'test', 'true'}]}].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_event(kz_json:object(), ne_binary()) -> kz_json:object().
format_event(JObj, _AccountId) ->
    kz_json:normalize(JObj).
