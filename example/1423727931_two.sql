-- :up
ALTER TABLE users RENAME TO clients;

-- :down
ALTER TABLE clients RENAME TO users;
