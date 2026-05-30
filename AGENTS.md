# AGENTS.md

## Project Overview

`orville-sqlite` is a Haskell library providing a type-safe SQLite API modeled after the [Flipstone Orville](https://github.com/flipstone/orville/) PostgreSQL library. It maps Haskell data types to SQLite tables with compile-time schema guarantees.

## Development Environment

**All development happens inside Docker.** No local Haskell tools should be installed directly on the host machine.

Use the `./hs` wrapper script for all Haskell tooling:

```bash
./hs stack build
./hs stack test
./hs stack ghci
./hs fourmolu -i src/
./hs hlint src/
```

The script builds a Docker image based on `ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.3-5d6640d` with `libsqlite3-dev` installed. A named Docker volume (`orville-sqlite-stack-root`) caches GHC and dependencies across worktrees.

## Build System

- **GHC**: 9.10.3
- **Build tool**: Stack (via the `hs` wrapper)
- **Formatter**: fourmolu
- **Linter**: hlint

## Dependencies

- **Base SQLite library**: [direct-sqlite](https://github.com/IreneKnapp/direct-sqlite) — the low-level FFI binding to SQLite
- This library builds type-safe marshalling, auto-migration, and query execution on top of `direct-sqlite`

## Target API

The library should provide APIs analogous to three key modules from `orville-postgresql`:

1. **`Orville.PostgreSQL.Marshall.SqlMarshaller`** — Type-safe mapping between Haskell records and SQL table columns
2. **`Orville.PostgreSQL.AutoMigration`** — Schema migration: create tables if they don't exist, alter tables to match expected schema
3. **`Orville.PostgreSQL.Execution`** — Query execution: SELECT, INSERT, UPDATE against typed tables

### Example Usage (Target API)

Given a Haskell type:

```haskell
data Person = Person
  { firstName :: Text
  , lastName  :: Text
  , age       :: Int
  }
```

It should map to a table:

```sql
CREATE TABLE person (
    first_name VARCHAR(100) NOT NULL,
    last_name  VARCHAR(100) NOT NULL,
    age        INT
);
```

And support:
- Creating the table if it doesn't exist
- Altering the table to match the schema when columns differ
- Querying rows as `Person` values
- Inserting `Person` values
- Updating rows from `Person` values

## Design Principles

- Haskell types to SQLite schema — not a direct port of the PostgreSQL implementation
- Follow the spirit and API shape of Orville, adapted to SQLite's type system and SQL dialect
- Favor type safety and compile-time guarantees over runtime flexibility
