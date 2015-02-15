-module(pgsql_migration).
-export([migrate/2, migrate/3, migrations/1, use_driver/1]).
-define(DRIVER, (application:get_env(pgsql_migration, driver, pgsql))).

migrate(Conn, Dir) ->
    migrate(Conn, integer_to_list(now_ts()), Dir).

migrate(Conn, Version, Dir) ->
    Migrations = migrations(Dir),
    BinVersion = list_to_binary(Version),
    case ?DRIVER:squery(Conn, "SELECT id FROM migrations ORDER BY id DESC") of
        {error, {error, error, <<"42P01">>, _, _}} ->
            %% init migrations and restart
            init_migrations(Conn),
            migrate(Conn, Version, Dir);
        {ok, _, [{BinVersion} | _]} ->
            up_to_date;
        {ok, _, [{Top} | _]} when Top < BinVersion ->
            %% upgrade path
            FromVersion = binary_to_list(Top),
            Upgraded = lists:foldl(fun({V, UpDown}, Acc) ->
                if
                    V =< Version andalso V > FromVersion ->
                        up(Conn, V, UpDown), [V | Acc];
                    true ->
                        Acc
                end
            end, [], Migrations),
            {up, lists:reverse(Upgraded)};
        {ok, _, [{Top}|_]} when Top > BinVersion ->
            %% downgrade path
            Downgraded = lists:foldl(fun({V, UpDown}, Acc) ->
                if
                    V >= Version ->
                        down(Conn, V, UpDown), [V | Acc];
                    true ->
                        Acc
                end
            end, [], Migrations),
            {down, lists:reverse(Downgraded)};
        {ok, _, []} ->
            %% full upgrade path
            Upgraded = lists:foldl(fun({V, UpDown}, Acc) ->
                if
                    V =< Version -> up(Conn, V, UpDown), [V | Acc];
                    true -> Acc
                end
            end, [], Migrations),
            {up, lists:reverse(Upgraded)}
    end.

migrations(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:map(fun(Filename) ->
        {ok, Migs} = eql:compile(filename:join([Dir, Filename])),
        {version_from_filename(Filename), Migs}
    end, lists:usort(Files)).

use_driver(Name) ->
    application:set_env(pgsql_migration, driver, Name).

%%
%% Private
%%

up(Conn, Version, UpDown) ->
    Query = eql:get_query(up, UpDown),
    if_ok(Version, ?DRIVER:squery(Conn, Query), fun() ->
        update_version(up, Conn, Version)
    end).

down(Conn, Version, UpDown) ->
    Query = eql:get_query(down, UpDown),
    if_ok(Version, ?DRIVER:squery(Conn, Query), fun() ->
        update_version(down, Conn, Version)
    end).

if_ok(_, R, Fn) when is_list(R) -> Fn();
if_ok(_, {ok, _}, Fn) -> Fn();
if_ok(_, {ok, _, _}, Fn) -> Fn();
if_ok(_, {ok, _, _, _}, Fn) -> Fn();
if_ok(V, {error, Reason}, _) ->
    throw(lists:flatten(format_error(V, Reason))).

format_error(V, {error, _, _, Msg, _}) ->
    io_lib:format("migration_error (~s): ~s", [V, Msg]);
format_error(V, Reason) ->
    io_lib:format("migration_error (~s): ~p", [V, Reason]).

version_from_filename(Filename) ->
    filename:rootname(Filename).

update_version(up, Conn, V) ->
    ?DRIVER:equery(Conn, "INSERT INTO migrations (id) VALUES ($1)", [V]);
update_version(down, Conn, V) ->
    ?DRIVER:equery(Conn, "DELETE FROM migrations WHERE id = $1", [V]).

init_migrations(Conn) ->
    {ok, _, _} = ?DRIVER:squery(Conn,
        "CREATE TABLE migrations ("
        "id VARCHAR(255) PRIMARY KEY,"
        "datetime TIMESTAMP DEFAULT CURRENT_TIMESTAMP)").

now_ts() ->
    {Mega, Seconds, _} = now(),
    Mega * 1000000 + Seconds.
