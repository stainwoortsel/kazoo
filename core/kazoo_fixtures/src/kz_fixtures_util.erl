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

%% API
-export([open_json/2, doc_path/2
        ,open_attachment/3, att_path/3

        ,update_revision/1
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

%%%===================================================================
%%% API
%%%===================================================================

-spec open_json(db_map(), ne_binary()) -> doc_resp().
open_json(Db, DocId) ->
    case read_file(doc_path(Db, DocId)) of
        {ok, Bin} -> {ok, kz_json:decode(Bin)};
        {error, _} -> {error, not_found}
    end.

-spec doc_path(db_map(), ne_binary()) -> file:filename_all().
doc_path(#{server := #{url := Url}, name := DbName}, DocId) ->
    filename:join(kz_term:to_list(Url) ++ kz_term:to_list(DbName)
                 ,["docs/", kz_term:to_list(DocId), ".json"]
                 ).

-spec open_attachment(db_map(), ne_binary(), ne_binary()) -> {ok, binary()} | {error, not_found}.
open_attachment(Db, DocId, AName) ->
    read_file(att_path(Db, DocId, AName)).

-spec att_path(db_map(), ne_binary(), ne_binary()) -> file:filename_all().
att_path(#{server := #{url := Url}, name := DbName}, DocId, AName) ->
    AttName = kz_http_util:urlencode(AName),
    filename:join(kz_term:to_list(Url) ++ kz_term:to_list(DbName)
                 ,["docs/", kz_term:to_list(DocId), AttName]
                 ).

-spec update_revision(kz_json:object()) -> kz_json:object().
update_revision(JObj) ->
    case kz_json:get_value(<<"_rev">>, JObj) of
        'undefined' ->
            kz_doc:set_revision(JObj, <<"1-", (kz_binary:rand_hex(16))/binary>>);
        Rev ->
            [RevPos|_] = binary:split(Rev, <<"-">>),
            Rev2 = kz_term:to_integer(RevPos) + 1,
            kz_doc:set_revision(JObj, <<(kz_term:to_binary(Rev2))/binary, "-", (kz_binary:rand_hex(16))/binary>>)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec read_file(file:filename_all()) -> {ok, binary()} | {error, not_found}.
read_file(Path) ->
    case file:read_file(Path) of
        {ok, _}=OK -> OK;
        {error, _} -> {error, not_found}
    end.
