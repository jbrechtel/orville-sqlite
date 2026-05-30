# orville-sqlite MVP Design

**Date**: 2026-05-29  
**Status**: Draft  
**Scope**: Minimal Viable Product — type-safe SQLite API modeled after [Flipstone Orville](https://github.com/flipstone/orville/)

---

## Overview

`orville-sqlite` is a Haskell library providing a type-safe SQLite API modeled after the Flipstone Orville PostgreSQL library. It maps Haskell data types to SQLite tables with compile-time schema guarantees.

The MVP covers three layers:
1. **FieldDefinition & SqlType** — type-safe column mapping
2. **SqlMarshaller & TableDefinition** — record-to-table mapping with primary keys
3. **AutoMigration** — automatic schema creation and column migration
4. **Execution** — basic CRUD operations

### What's NOT in MVP

- Foreign keys
- Indexes (beyond implicit primary key indexes)
- Composite primary keys
- Table comments
- Plans (N+1 query prevention)
- Batch operations
- Upsert
- `findEntitiesBy` (filter on non-PK columns)
- `updateFields` (partial update)
- `marshallPartial`, `marshallQualifyFields`, `prefixMarshaller`
- `AnnotatedSqlMarshaller` / `marshallResultFromSql`
- `SyntheticField`
- Nested record marshalling
- Raw SQL execution (`executeAndDecode`)

---

## Module Structure

Single package `orville-sqlite`. Flat module hierarchy under `Orville.SQLite.*`.

```
src/
  Orville/SQLite.hs
  Orville/SQLite/Monad.hs
  Orville/SQLite/RawSql.hs
  Orville/SQLite/SqlType.hs
  Orville/SQLite/FieldDefinition.hs
  Orville/SQLite/SqlMarshaller.hs
  Orville/SQLite/TableDefinition.hs
  Orville/SQLite/AutoMigration.hs
  Orville/SQLite/Execution.hs
```

**Dependencies**: `base >=4.17 && <5`, `direct-sqlite`, `text`, `bytestring`

**Comparison to Orville PostgreSQL**: Drops ~60+ submodules. No Expression sub-hierarchy, no Execution sub-hierarchy, no extensions/sequences/triggers/indexes/windows/locking.

---

## Component Designs

### 1. SqlType (`Orville.SQLite.SqlType`)

SQLite has 5 storage classes: NULL, INTEGER, REAL, TEXT, BLOB. The `SqlType` encodes how Haskell values convert to/from SQLite values.

```haskell
data SqlType a = SqlType
  { sqlTypeName   :: String           -- "INTEGER", "TEXT", "BLOB", "REAL"
  , sqlTypeToSql  :: a -> SqlValue    -- Haskell → SQLite value
  , sqlTypeFromSql :: SqlValue -> Either String a  -- SQLite value → Haskell
  }
```

Pre-built instances:
- `integerType :: SqlType Int64`
- `textType :: SqlType Text`
- `realType :: SqlType Double`
- `blobType :: SqlType ByteString`

Helper: `convertSqlType :: (a -> b) -> (b -> a) -> SqlType a -> SqlType b` for wrapping custom types.

### 2. FieldDefinition (`Orville.SQLite.FieldDefinition`)

A GADT parameterized by nullability. Maps a Haskell type to a named SQL column.

```haskell
data Nullability = NotNull | Nullable

data FieldDefinition (nullability :: Nullability) a where
  FieldDefinition ::
    { fieldName :: String
    , fieldSqlType :: SqlType a
    } -> FieldDefinition 'NotNull a
  NullableFieldDefinition ::
    { nullableFieldName :: String
    , nullableFieldSqlType :: SqlType a
    } -> FieldDefinition 'Nullable a
```

Construction functions:
- `integerField :: String -> FieldDefinition 'NotNull Int64`
- `textField :: String -> FieldDefinition 'NotNull Text`
- `realField :: String -> FieldDefinition 'NotNull Double`
- `blobField :: String -> FieldDefinition 'NotNull ByteString`
- `nullableField :: FieldDefinition 'NotNull a -> FieldDefinition 'Nullable a`
- `convertField :: (a -> b) -> (b -> a) -> FieldDefinition null a -> FieldDefinition null b`

### 3. SqlMarshaller (`Orville.SQLite.SqlMarshaller`)

A GADT combining field definitions into record-level marshalling using applicative syntax. This is the core type-safe mapping between Haskell records and SQLite tables.

```haskell
data SqlMarshaller writeEntity readEntity where
  MarshallPure   :: readEntity -> SqlMarshaller writeEntity readEntity
  MarshallApply  :: SqlMarshaller writeEntity (a -> b)
                 -> SqlMarshaller writeEntity a
                 -> SqlMarshaller writeEntity b
  MarshallNest   :: (writeEntity -> a)
                 -> SqlMarshaller a readEntity
                 -> SqlMarshaller writeEntity readEntity
  MarshallField  :: FieldDefinition nullability a -> SqlMarshaller a a
```

Combinators:
- `marshallField :: (writeEntity -> a) -> FieldDefinition 'NotNull a -> SqlMarshaller writeEntity a` — read+write column
- `marshallReadOnlyField :: FieldDefinition nullability a -> SqlMarshaller writeEntity a` — for DB-populated columns (e.g., auto-increment PK). Excluded from INSERT/UPDATE, included in SELECT.
- `marshallMaybe :: FieldDefinition 'Nullable a -> SqlMarshaller writeEntity (Maybe a)` — wraps nullable column as `Maybe`, handles SQL NULL.

Internal operations (not exposed as public API):
- Collect derived columns for SELECT
- Encode writeEntity to `[(String, SqlValue)]` for INSERT/UPDATE
- Decode a result row into readEntity
- Enumerate `FieldDefinition`s for AutoMigration schema comparison

Usage pattern:
```haskell
data Person = Person { firstName :: Text, lastName :: Text, age :: Int64 }

personMarshaller :: SqlMarshaller Person Person
personMarshaller =
  Person
    <$> marshallField firstName firstNameField
    <*> marshallField lastName  lastNameField
    <*> marshallField age       ageField
```

### 4. TableDefinition (`Orville.SQLite.TableDefinition`)

Ties a table name, primary key, and marshaller together.

```haskell
data PrimaryKey writeEntity key where
  PrimaryKey ::
    (writeEntity -> key) ->               -- accessor to extract key from writeEntity
    FieldDefinition 'NotNull key ->        -- column definition
    PrimaryKey writeEntity key

data TableDefinition key writeEntity readEntity = TableDefinition
  { tableName :: String
  , tablePrimaryKey :: PrimaryKey writeEntity key
  , tableMarshaller :: SqlMarshaller writeEntity readEntity
  }
```

The `PrimaryKey` includes an accessor function so that `updateEntity` and `deleteEntity` can extract the key value from the `writeEntity` for the WHERE clause, even when the PK field is mapped as read-only in the marshaller.

Construction:
- `primaryKey :: (writeEntity -> key) -> FieldDefinition 'NotNull key -> PrimaryKey writeEntity key`
- `mkTableDefinition :: String -> PrimaryKey writeEntity key -> SqlMarshaller writeEntity readEntity -> TableDefinition key writeEntity readEntity`
- `mkTableDefinitionWithoutKey :: String -> SqlMarshaller w r -> TableDefinition () w r`

### 5. AutoMigration (`Orville.SQLite.AutoMigration`)

Compares expected schema (from `TableDefinition`) against actual SQLite schema (via `PRAGMA table_info`) and generates DDL.

#### API

```haskell
data MigrationOptions = MigrationOptions
  { runSchemaChanges :: Bool    -- True = apply changes, False = plan only
  }

defaultOptions :: MigrationOptions
defaultOptions = MigrationOptions { runSchemaChanges = True }

data SchemaItem where
  SchemaTable :: TableDefinition key writeEntity readEntity -> SchemaItem

autoMigrateSchema :: MigrationOptions -> [SchemaItem] -> OrvilleM ()

generateMigrationPlan :: [SchemaItem] -> OrvilleM [MigrationStep]

executeMigrationPlan :: [MigrationStep] -> OrvilleM ()

data MigrationStep
  = CreateTable String [(String, String, Bool)] (String, String)
    -- table name, [(colName, colType, notNull)], (pkName, pkType)
  | AddColumn String String String Bool
    -- table name, colName, colType, notNull
  | DropColumn String String
    -- table name, colName
```

Explicit column drop opt-in (mirroring Orville):
```haskell
dropColumns :: [String] -> TableDefinition key w r -> SchemaItem
```

#### Schema Comparison Logic

1. Query `PRAGMA table_info(<tableName>)` which returns `(cid, name, type, notnull, dflt_value, pk)`
2. **Table missing** → generate `CREATE TABLE IF NOT EXISTS` with all expected columns and primary key constraint
3. **Column missing in DB but expected** → generate `ALTER TABLE ... ADD COLUMN`. The column must be nullable. If the ColumnDefinition specifies NOT NULL, error with guidance suggesting multi-step migration.
4. **Column in DB but not expected** → ignored (kept). Columns are only dropped with explicit `dropColumns` opt-in.
5. **Column type or nullability mismatch** → error with guidance explaining the mismatch and suggesting multi-step migration approach.

#### Error Handling for Incompatible Changes

When a column type or nullability changes in a way SQLite can't handle via ALTER TABLE:
- Error message includes: table name, column name, expected vs actual type/nullability
- Suggestion: "Consider a multi-step migration: add a new nullable column, backfill data, then use dropColumns to remove the old column"

#### SQLite DDL Constraints (Informing Design)

- `ALTER TABLE` can: rename table, add column (to end), rename column, drop column
- `ALTER TABLE` cannot: change column type, change NOT NULL constraint, remove column without explicit drop
- Added columns must be nullable or have a default value

### 6. Execution (`Orville.SQLite.Execution`)

Basic CRUD operations running in `OrvilleM`.

#### API

```haskell
-- INSERT
insertEntity :: TableDefinition key w r -> w -> OrvilleM ()

-- SELECT
findEntity :: TableDefinition key w r -> key -> OrvilleM (Maybe r)
findAll :: TableDefinition key w r -> OrvilleM [r]

-- UPDATE
updateEntity :: TableDefinition key w r -> w -> OrvilleM ()

-- DELETE
deleteEntity :: TableDefinition key w r -> key -> OrvilleM ()
```

#### How Operations Work

**insertEntity**: Extracts `(colName, sqlValue)` pairs from writeEntity via marshaller (excluding read-only fields), builds `INSERT INTO table (cols) VALUES (vals)`, binds values as parameters, executes via `direct-sqlite`.

**findEntity**: Uses marshaller to generate `SELECT cols FROM table WHERE pk = ?`, binds the key value, decodes the single result row. Returns `Nothing` if no row found.

**findAll**: Generates `SELECT cols FROM table`, decodes all rows.

**updateEntity**: Generates `UPDATE table SET col=val, ... WHERE pk = val`. Only non-read-only fields from the marshaller are included in the SET clause. The PK value for the WHERE clause is extracted from writeEntity via `PrimaryKey`'s accessor function (which works even when the PK is mapped as read-only in the marshaller for auto-increment columns).

**deleteEntity**: `DELETE FROM table WHERE pk = ?` — the PK value is extracted from the provided key argument via the `PrimaryKey`'s `FieldDefinition` for encoding.

#### Error Handling

Uses `MarshallError` for decoding failures — identifies which column failed and why (type mismatch, out of range, etc.).

### 7. Monad (`Orville.SQLite.Monad`)

A simple reader monad wrapping a `direct-sqlite` `Database` handle.

```haskell
newtype OrvilleM a = OrvilleM (ReaderT Database IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Database)

withConnection :: Database -> OrvilleM a -> IO a
openConnection :: String -> IO Database
closeConnection :: Database -> IO ()
```

No connection pool. No `OrvilleState`. No transaction callbacks. For MVP, transactions are explicit:
```haskell
withTransaction :: OrvilleM a -> OrvilleM a
```

---

## Example Usage (Target API)

```haskell
import Orville.SQLite

data Person = Person
  { personId :: Int64
  , firstName :: Text
  , lastName :: Text
  , age :: Int64
  }

personIdField :: FieldDefinition 'NotNull Int64
personIdField = integerField "id"

firstNameField :: FieldDefinition 'NotNull Text
firstNameField = textField "first_name"

lastNameField :: FieldDefinition 'NotNull Text
lastNameField = textField "last_name"

ageField :: FieldDefinition 'NotNull Int64
ageField = integerField "age"

personMarshaller :: SqlMarshaller Person Person
personMarshaller =
  Person
    <$> marshallReadOnlyField personIdField
    <*> marshallField firstName firstNameField
    <*> marshallField lastName  lastNameField
    <*> marshallField age       ageField

personTable :: TableDefinition Int64 Person Person
personTable =
  mkTableDefinition "person" (primaryKey personId personIdField) personMarshaller

main :: IO ()
main = do
  db <- openConnection "people.db"
  withConnection db $ do
    autoMigrateSchema defaultOptions [SchemaTable personTable]
    insertEntity personTable (Person 0 "Alice" "Smith" 30)   -- 0 is ignored (read-only PK)
    mAlice <- findEntity personTable 1
    -- mAlice = Just (Person 1 "Alice" "Smith" 30)
    updateEntity personTable (Person 1 "Alice" "Smith" 31)  -- PK extracted for WHERE
    pure ()
  closeConnection db
```

---

## Implementation Order

1. **Monad + RawSql** — Foundation: `OrvilleM`, `withConnection`, `openConnection`, minimal raw SQL builder
2. **SqlType** — Type encoding/decoding for SQLite storage classes
3. **FieldDefinition** — GADT with `NotNull`/`Nullable`, construction functions
4. **SqlMarshaller** — GADT with applicative combinators, encode/decode/column listing internals
5. **TableDefinition** — `mkTableDefinition`, `PrimaryKey`
6. **AutoMigration** — `PRAGMA table_info` introspection, schema comparison, DDL generation
7. **Execution** — `insertEntity`, `findEntity`, `findAll`, `updateEntity`, `deleteEntity`
8. **Top-level re-exports** — `Orville.SQLite` module
