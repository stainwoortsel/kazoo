%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_fixtures_view).

%% View-related
-export([design_info/3
        ,all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,all_docs/3
        ]).

-include("kz_fixtures.hrl").

-spec design_info(server_map(), ne_binary(), ne_binary()) -> doc_resp().
design_info(_Server, _DbName, _Design) ->
    {error, <<"not_me">>}.

-spec all_design_docs(server_map(), ne_binary(), kz_data:options()) -> docs_resp().
all_design_docs(_Server, _DbName, _Options) ->
    {error, <<"leave_me_alone">>}.

-spec get_results(server_map(), ne_binary(), ne_binary(), kz_data:options()) -> docs_resp().
get_results(_Server, _DbName, _Design, _Options) ->
    {ok, []}.

-spec get_results_count(server_map(), ne_binary(), ne_binary(), kz_data:options()) -> {ok, non_neg_integer()} | fixture_error().
get_results_count(_Server, _DbName, _Design, _Options) ->
    {error, <<"why_are_you_calling_me">>}.

-spec all_docs(server_map(), ne_binary(), kz_data:options()) -> docs_resp().
all_docs(_Server, _DbName, _Options) ->
    {error, <<"last_response_i'm_out">>}.
