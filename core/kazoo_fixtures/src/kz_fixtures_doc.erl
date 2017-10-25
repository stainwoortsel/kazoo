%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_fixtures_doc).

%% Document operations
-export([open_doc/4
        ,lookup_doc_rev/3
        ,save_doc/4
        ,save_docs/4
        ,del_doc/4
        ,del_docs/4
        ,ensure_saved/4
        ]).

-include("kz_fixtures.hrl").

%%%===================================================================
%%% Document operations
%%%===================================================================

-spec open_doc(server_map(), ne_binary(), ne_binary(), kz_data:options()) -> doc_resp().
open_doc(Server, DbName, DocId, _Options) ->
    Db = kz_fixtures_server:get_db(Server, DbName),
    case kz_term:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> kz_fixtures_util:open_json(Db, DocId)
    end.

-spec lookup_doc_rev(server_map(), ne_binary(), ne_binary()) -> {ok, ne_binary()} | fixture_error().
lookup_doc_rev(Server, DbName, DocId) ->
    case open_doc(Server, DbName, DocId, []) of
        {'ok', Doc} -> {'ok', kz_doc:rev(Doc)};
        {'error', _}=Error -> Error
    end.

-spec save_doc(server_map(), ne_binary(), kz_data:document() | kz_data:documents(), kz_data:options()) -> doc_resp().
save_doc(Server, DbName, Docs, Options) when is_list(Docs) ->
    save_docs(Server, DbName, Docs, Options);
save_doc(Server, DbName, Doc, Options) ->
    case open_doc(Server, DbName, Doc, Options) of
        {'ok', JObj} ->
            DocRev = kz_doc:revision(Doc),
            JObjRev = kz_doc:revision(JObj),
            case {DocRev, JObjRev} of
                {'undefined', _} -> kz_fixtures_util:update_revision(kz_data:set_revision(Doc, JObjRev));
                {_, 'undefined'} -> kz_fixtures_util:update_revision(Doc);
                {DocRev, JObjRev} -> kz_fixtures_util:update_revision(Doc);
                {_, _} -> {'error', 'conflict'}
            end;
        {'error', _} ->
            case kz_fixtures_db:db_exists(Server, DbName) of
                'false' -> {'error', 'not_found'};
                'true' -> {'ok', kz_fixtures_util:update_revision(Doc)}
            end
    end.

-spec save_docs(server_map(), ne_binary(), kz_data:documents(), kz_data:options()) -> docs_resp().
save_docs(Server, DbName, Docs, Options) ->
    case kz_fixtures_db:db_exists(Server, DbName) of
        'false' -> {'error', 'not_found'};
        'true' ->
            {'ok', [perform_save_docs(Server, DbName, Doc, Options) || Doc <- Docs]}
    end.

-spec del_doc(server_map(), ne_binary(), kz_data:document(), kz_data:options()) -> doc_resp().
del_doc(Server, DbName, Doc, Options) ->
    del_docs(Server, DbName, [Doc], Options).

-spec del_docs(server_map(), ne_binary(), kz_data:documents(), kz_data:options()) -> docs_resp().
del_docs(Server, DbName, Docs, Options) ->
    [perform_save_docs(Server, DbName, Doc, Options) || Doc <- Docs].

-spec ensure_saved(server_map(), ne_binary(), kz_data:document(), kz_data:options()) -> doc_resp().
ensure_saved(Server, DbName, Doc, Options) ->
    case save_doc(Server, DbName, Doc, Options) of
        {'ok', _}=OK -> OK;
        {'error', 'conflict'} ->
            case lookup_doc_rev(Server, DbName, kz_doc:id(Doc)) of
                {'ok', Rev} -> ensure_saved(Server, DbName, kz_doc:set_revision(Doc, Rev), Options);
                {'error', 'not_found'} -> ensure_saved(Server, DbName, kz_doc:delete_revision(Doc), Options)
            end;
        {'error', _}=Error -> Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec perform_save_docs(server_map(), ne_binary(), kz_data:document(), kz_data:options()) -> kz_json:objects().
perform_save_docs(Server, DbName, JObj, Options) ->
    prepare_bulk_save_response(save_doc(Server, DbName, JObj, Options), JObj).

-spec prepare_bulk_save_response(doc_resp(), kz_json:object()) -> kz_json:object().
prepare_bulk_save_response({'ok', JObj}, _) ->
    kz_json:from_list(
      [{<<"ok">>, 'true'}
      ,{<<"id">>, kz_doc:id(JObj)}
      ,{<<"rev">>, kz_doc:revision(JObj)}
      ,{<<"accepted">>, 'true'}
      ]);
prepare_bulk_save_response({'error', Error}, JObj) ->
    Reason = kz_term:safe_cast(Error, <<"unknown">>, fun kz_term:to_binary/1),
    kz_json:from_list(
      [{<<"id">>, kz_doc:id(JObj)}
      ,{<<"error">>, Reason}
      ,{<<"reason">>, <<"Document update failed due to error ", Reason/binary>>}
      ]).
