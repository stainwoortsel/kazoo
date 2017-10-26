%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_fixturedb_util).

%% Driver callbacks
-export([format_error/1
        ]).

%% API
-export([open_json/2, doc_path/2
        ,open_attachment/3, att_path/3
        ,open_view/3, view_path/3

        ,docs_dir/1, views_dir/1

        ,update_revision/1

        ,encode_query_filename/2
        ]).

-include("kz_fixturedb.hrl").

%%%===================================================================
%%% Driver callbacks
%%%===================================================================

-spec format_error(any()) -> any().
format_error('timeout') -> 'timeout';
format_error('conflict') -> 'conflict';
format_error('not_found') -> 'not_found';
format_error('db_not_found') -> 'db_not_found';
format_error(Other) -> Other.

%%%===================================================================
%%% API
%%%===================================================================

-spec open_json(db_map(), ne_binary()) -> doc_resp().
open_json(Db, DocId) ->
    read_json(doc_path(Db, DocId)).

-spec doc_path(db_map(), ne_binary()) -> file:filename_all().
doc_path(#{server := #{url := Url}, name := DbName}, DocId) ->
    filename:join(kz_term:to_list(Url) ++ "/" ++ kz_term:to_list(DbName)
                 ,["docs/", http_uri:encode(kz_term:to_list(DocId)), ".json"]
                 ).

-spec open_attachment(db_map(), ne_binary(), ne_binary()) -> {ok, binary()} | {error, not_found}.
open_attachment(Db, DocId, AName) ->
    read_file(att_path(Db, DocId, AName)).

-spec att_path(db_map(), ne_binary(), ne_binary()) -> file:filename_all().
att_path(#{server := #{url := Url}, name := DbName}, DocId, AName) ->
    AttName = http_uri:encode(AName),
    filename:join(kz_term:to_list(Url) ++ "/" ++ kz_term:to_list(DbName)
                 ,["docs/", http_uri:encode(kz_term:to_list(DocId)), AttName]
                 ).

-spec open_view(db_map(), ne_binary(), kz_data:options()) -> docs_resp().
open_view(Db, Design, Options) ->
    read_json(view_path(Db, Design, Options)).

-spec view_path(db_map(), ne_binary(), kz_data:options()) -> file:filename_all().
view_path(#{server := #{url := Url}, name := DbName}, Design, Options) ->
    filename:join(kz_term:to_list(Url) ++ "/" ++ kz_term:to_list(DbName)
                 ,["views/", encode_query_filename(Design, Options)]
                 ).

%% @doc
%% The idea is to encode file name based on view options so you can
%% write JSON file specifically for each of your view queries
-spec encode_query_filename(ne_binary(), kz_data:options()) -> ne_binary().
encode_query_filename(Design, Options) ->
    encode_query_options(Design, ?DANGEROUS_VIEW_OPTS, Options, []).

-spec docs_dir(db_map()) -> text().
docs_dir(#{server := #{url := Url}, name := DbName}) ->
    kz_term:to_list(<<Url/binary, "/", DbName/binary, "/docs">>).

-spec views_dir(db_map()) -> text().
views_dir(#{server := #{url := Url}, name := DbName}) ->
    kz_term:to_list(<<Url/binary, "/", DbName/binary, "/views">>).

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

-spec read_json(file:filename_all()) -> doc_resp().
read_json(Path) ->
    case read_file(Path) of
        {ok, Bin} -> {ok, kz_json:decode(Bin)};
        {error, _} -> {error, not_found}
    end.

-spec read_file(file:filename_all()) -> {ok, binary()} | {error, not_found}.
read_file(Path) ->
    case file:read_file(Path) of
        {ok, _}=OK -> OK;
        {error, _} -> {error, not_found}
    end.

-spec encode_query_options(ne_binary(), list(), kz_data:options(), list()) -> text().
encode_query_options(Design, [], _, []) ->
    DesignView = design_view(Design),
    kz_term:to_list(<<DesignView/binary, ".json">>);
encode_query_options(Design, [], _, Acc) ->
    DesignView = design_view(Design),
    QueryHash = kz_binary:hexencode(crypto:hash(md5, erlang:term_to_binary(Acc))),

    kz_term:to_list(<<DesignView/binary, "-", QueryHash/binary, ".json">>);
encode_query_options(Design, [Key|Keys], Options, Acc) ->
    case props:get_value(Key, Options, not_defined) of
        not_defined -> encode_query_options(Design, Keys, Options, Acc);
        Value -> encode_query_options(Design, Keys, Options, ["&", Key, "=", Value | Acc])
    end.

-spec design_view(ne_binary()) -> ne_binary().
design_view(Design) ->
    case binary:split(Design, <<"/">>) of
        [DesignName] -> DesignName;
        [DesignName, ViewName|_] -> <<DesignName/binary, "+", ViewName/binary>>
    end.
