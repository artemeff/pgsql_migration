### PostgreSQL migrations for Erlang

---

Simple utility to add migrations for Erlang projects. It doesn't have any database abstraction, you'll still operate with plain SQL and it only supports PostgreSQL (using pgsql or epgsql library). _Inspired by [erlang-sql-migrations](https://github.com/spawngrid/erlang-sql-migrations) by spawngrid_.

---

#### Use

In order to use the tool one writes 'migration' files. Most important thing here is to name in the order of their versioning, like this: `[timestamp]_name.sql`.

```sql
-- priv/migrations/1423733547_users.sql
-- :up
CREATE TABLE users();

-- :down
DROP TABLE users;
```

```erlang
pgsql_migraion:migrate(Conn, "priv/migrations").
```

You can rename pgsql driver, if you use [epgsql](https://github.com/epgsql/epgsql) 3.0 or newer:

```erlang
pgsql_migraion:use_driver(epgsql).
```

---

### Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
