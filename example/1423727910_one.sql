-- Create users table
-- :up
CREATE TABLE users(
  id serial NOT NULL,
  login character varying(255) NOT NULL,
  password character varying(255) NOT NULL,
  CONSTRAINT users_pkey PRIMARY KEY (id)
);

-- create index
CREATE INDEX index_users_on_login
  ON users
  USING btree
  (login COLLATE pg_catalog."default");

-- Drop users in downgrade
-- :down
DROP TABLE users;
