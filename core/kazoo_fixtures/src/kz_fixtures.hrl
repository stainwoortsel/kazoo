-ifndef(KZ_FIXTURES_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-type fixture_errors() :: not_found | timeout |
						  conflict | db_not_found.
-type fixture_error() :: {error, fixture_errors()}.

-type doc_resp() :: {ok, kz_json:object()} | fixture_error().
-type docs_resp() :: {ok, kz_json:objects()} | fixture_error().

-define(KZ_FIXTURES_HRL, 'true').
-endif.
