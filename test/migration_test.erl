-module(migration_test).
-include_lib("eunit/include/eunit.hrl").

migration_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun working/0
    , fun failing/0
    ]}.

start() -> connect().
stop(_) -> close().

%%
%% Tests
%%

working() ->
    C = get_c(),
    ?assertMatch({up, [{"1423994257_create", ok}, {"1423994258_insert", ok}]},
        pgsql_migration:migrate(C, dir("working"))),
    ?assertMatch({ok, _, [{<<"123">>}|_]},
        epgsql:squery(C, "SELECT * FROM working")).

failing() ->
    C = get_c(),
    ?assertException(throw, _,
        pgsql_migration:migrate(C, dir("failing"))),
    ?assertMatch({error, _},
        epgsql:squery(C, "SELECT * FROM failing")),
    ?assertMatch({error, _},
        epgsql:squery(C, "SELECT * FROM failing2")).

%%
%% Helpers
%%

dir(Append) ->
    {ok, C} = file:get_cwd(),
    filename:join([C, "..", "test", Append]).

connect() ->
    ets:new(conf, [set, named_table]),
    Opts = [{database, "pgsql_migration"}],
    pgsql_migration:use_driver(epgsql),
    case epgsql:connect("localhost", "postgres", "", Opts) of
        {ok, C} ->
            ets:insert(conf, {conn, C}),
            truncate();
        _ ->
            throw(connection_error)
    end.

get_c() ->
    [{conn, C}|_] = ets:lookup(conf, conn), C.

close() ->
    truncate(),
    ok = epgsql:close(get_c()).

truncate() ->
    epgsql:squery(get_c(), "drop schema public cascade; create schema public;").
