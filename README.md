# orville-sqlite

A type-safe SQLite API for Haskell, modeled after the [Flipstone Orville](https://github.com/flipstone/orville/) PostgreSQL library.

Maps Haskell data types to SQLite tables with compile-time schema guarantees.

## Status — MVP

The MVP provides the core pipeline: define columns and record marshallers, auto-migrate schemas, and execute basic CRUD queries.

## Usage

```haskell
{-# LANGUAGE DataKinds #-}

import Orville.SQLite

data Person = Person
  { personId :: Int64
  , firstName :: Text
  , lastName :: Text
  , age :: Int64
  }
  deriving (Show, Eq)

-- Column definitions (type-safe, parameterized by nullability)
personIdField :: FieldDefinition 'NotNull Int64
personIdField = integerField "id"

firstNameField :: FieldDefinition 'NotNull Text
firstNameField = textField "first_name"

lastNameField :: FieldDefinition 'NotNull Text
lastNameField = textField "last_name"

ageField :: FieldDefinition 'NotNull Int64
ageField = integerField "age"

-- SqlMarshaller: maps the record to/from SQL columns
personMarshaller :: SqlMarshaller Person Person
personMarshaller =
  Person
    <$> marshallReadOnlyField personIdField  -- auto-increment PK
    <*> marshallField firstName firstNameField
    <*> marshallField lastName lastNameField
    <*> marshallField age ageField

-- TableDefinition ties a table name, primary key, and marshaller together
personTable :: TableDefinition Int64 Person Person
personTable =
  mkTableDefinition "person" (primaryKey personId personIdField) personMarshaller

main :: IO ()
main = do
  db <- openConnection "people.db"
  withConnection db $ do
    -- Auto-migrate: creates the table if it doesn't exist
    autoMigrateSchema defaultOptions [schemaTable personTable []]

    -- Insert
    insertEntity personTable (Person 0 "Alice" "Smith" 30)
    insertEntity personTable (Person 0 "Bob"   "Jones" 25)

    -- Query by primary key
    mAlice <- findEntity personTable 1
    -- mAlice = Just (Person 1 "Alice" "Smith" 30)

    -- Find all
    all <- findAll personTable
    -- all = [Person 1 "Alice" "Smith" 30, Person 2 "Bob" "Jones" 25]

    -- Update
    updateEntity personTable (Person 1 "Alice" "Jones" 31)

    -- Delete
    deleteEntity personTable 2

    pure ()
  closeConnection db
```

The above creates this table:

```sql
CREATE TABLE IF NOT EXISTS person (
  age INTEGER NOT NULL,
  last_name TEXT NOT NULL,
  first_name TEXT NOT NULL,
  id INTEGER PRIMARY KEY
);
```

## Modules

| Module | Purpose |
|--------|---------|
| `Orville.SQLite` | Top-level re-exports |
| `Orville.SQLite.Monad` | `OrvilleM` reader monad and connection management |
| `Orville.SQLite.SqlType` | Type encoding/decoding for SQLite storage classes |
| `Orville.SQLite.FieldDefinition` | GADT column definitions with `NotNull`/`Nullable` |
| `Orville.SQLite.SqlMarshaller` | GADT record-to-table mapping (applicative syntax) |
| `Orville.SQLite.TableDefinition` | `PrimaryKey` and `mkTableDefinition` |
| `Orville.SQLite.AutoMigration` | Schema introspection via `PRAGMA table_info`, DDL generation |
| `Orville.SQLite.Execution` | `insertEntity`, `findEntity`, `findAll`, `updateEntity`, `deleteEntity` |

## Feature Coverage

### Included (MVP)

- `FieldDefinition` with `NotNull`/`Nullable` type parameters
- `SqlMarshaller` GADT with `marshallField`, `marshallReadOnlyField`, `marshallMaybe`
- `TableDefinition` with single-column primary keys
- `AutoMigration`: create tables, add nullable columns, drop columns (explicit opt-in), dry-run
- `Execution`: insert, find-by-PK, find-all, update, delete
- In-memory databases (`:memory:`) and file-based databases

### Not yet implemented

- Foreign keys and composite primary keys
- Table indexes
- Plans (N+1 query prevention via `Plan`)
- Batch operations
- Upsert (`upsertEntity`)
- `findEntitiesBy` (filtering on non-PK columns)
- `updateFields` (partial update)
- Raw SQL execution (`executeAndDecode`)
- Nested record marshalling and `prefixMarshaller`
- `AnnotatedSqlMarshaller`
- `SyntheticField`
- Connection pooling

## Development

All tooling runs inside Docker via the `./hs` wrapper. No local Haskell installation needed.

```bash
./hs stack build       # build
./hs stack test        # run tests (53 tests)
./hs stack ghci        # REPL
./hs fourmolu -i src/  # format
```

Or use the convenience scripts:

```bash
./scripts/build         # build with pedantic
./scripts/test          # run tests
```

### Dependencies

- GHC 9.10.3
- [direct-sqlite](https://github.com/IreneKnapp/direct-sqlite) — low-level FFI binding to SQLite
- `text`, `bytestring`, `mtl`
