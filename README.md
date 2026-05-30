# orville-sqlite

This library is intended to be a SQLite Haskell API similar in spirit to https://github.com/flipstone/orville/

The goal of this library is to provide a type safe approach to mapping Haskell types to SQLite data. We aim for type-safe queries and migrations.

The Flipstone orville library is robust and deep. This library aspires to the same but realizes it's unlikely to get there anytime soon.

## Goal

This library intends to provide a similar API for describing the database schema and executing queries as the Flipstone Orville library.

We will not port the PostgreSQL implementation directly but instead target a similar API.

The APIs that concern us the most are:

- Orville.PostgreSQL.Marshall.SqlMarshaller
- Orville.PostgreSQL.AutoMigration as AutoMigration
- Orville.PostgreSQL.Execution

and even for those, we only what's strictly necessary to define a way to take a Haskell data type like

```haskell

data Person =
  Person
    { firstName :: Text
    , lastName :: Text
    , age :: Int
    }
```

and map it to a table of schema

```sql
CREATE TABLE person (
    first_name VARCHAR(100) NOT NULL,
    last_name  VARCHAR(100) NOT NULL,
    age        INT
);
```

and then give us an API for marshalling that Haskell type to that table definition

then using that marshaller we should have APIs that can;

- create the table if it does not exist
- alter the table to match the schema if the table does exist but its columns don't match
- query the table and retrieve Person values
- insert Person values into the table
- update the table from Person values.

## Development environment

The development environment should be entirely inside of Docker - no local
tools should be installed on the machine directly. use the `hs` wrapper script
to execute Haskell tooling inside of Docker.

## Base SQLite library

Our base library for interacting with SQLite should be direct-sqlite - https://github.com/IreneKnapp/direct-sqlite
