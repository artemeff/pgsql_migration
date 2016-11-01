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
            %% support legacy error message arity
            init_migrations(Conn),
            migrate(Conn, Version, Dir);
        {error, {error, error, <<"42P01">>, _, _, _}} ->
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
                        [exec(up, Conn, V, UpDown) | Acc];
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
                        [exec(down, Conn, V, UpDown) | Acc];
                    true ->
                        Acc
                end
            end, [], Migrations),
            {down, lists:reverse(Downgraded)};
        {ok, _, []} ->
            %% full upgrade path
            Upgraded = lists:foldl(fun({V, UpDown}, Acc) ->
                if
                    V =< Version ->
                        [exec(up, Conn, V, UpDown) | Acc];
                    true ->
                        Acc
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

exec(Type, Conn, Version, UpDown) ->
    Query = eql:get_query(Type, UpDown),
    case if_ok(exec_transaction(Conn, Query)) of
        skip ->
            {Version, skip};
        ok ->
            update_version(Type, Conn, Version),
            commit_transaction(Conn),
            {Version, ok};
        {error, Reason} ->
            rollback_transaction(Conn),
            Err = io_lib:format("migration error: ~p~n", [Reason]),
            throw(lists:flatten(Err))
    end.

exec_transaction(_, undefined) -> skip;
exec_transaction(Conn, Query) ->
    ?DRIVER:squery(Conn, "BEGIN;"),
    ?DRIVER:squery(Conn, Query).

commit_transaction(Conn) ->
    ?DRIVER:squery(Conn, "COMMIT;").

rollback_transaction(Conn) ->
    ?DRIVER:squery(Conn, "ROLLBACK;").

if_ok(skip) -> skip;
if_ok(Rs) when is_list(Rs) ->
    Result = lists:map(fun(R) -> if_ok(R) end, Rs),
    case lists:keyfind(error, 1, Result) of
        false -> ok;
        Error -> Error
    end;
if_ok({ok, _}) -> ok;
if_ok({ok, _, _}) -> ok;
if_ok({ok, _, _, _}) -> ok;
if_ok(Error) -> {error, Error}.

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
    {Mega, Seconds, _} = os:timestamp(),
    Mega * 1000000 + Seconds.
