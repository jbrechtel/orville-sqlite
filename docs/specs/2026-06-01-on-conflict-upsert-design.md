# ON CONFLICT Support (Upsert / Do Nothing) Design

Date: 2026-06-01

## Overview

Add `ON CONFLICT` clause support to orville-sqlite, modeled after the
flipstone-orville PostgreSQL library. SQLite has native `ON CONFLICT DO UPDATE
SET` and `ON CONFLICT DO NOTHING` support since version 3.24.0 (2018).

## Module Structure

### New module: `Orville.SQLite.Expr.OnConflict`

SQL expression types for building `ON CONFLICT` clauses, modeled after
`Orville.PostgreSQL.Expr.OnConflict`.

- `OnConflictExpr` — newtype over `RawSql` representing the full `ON CONFLICT
  <target> <action>` clause.
- `ConflictTargetExpr` — newtype over `RawSql` representing just the target
  portion (the column list or expression inside the parentheses).
- `onConflictDoUpdate` — builds `ON CONFLICT <target> DO UPDATE SET col1 =
  excluded.col1, ...`. Takes the list of column names to set (using the
  `excluded.` pseudo-table).
- `onConflictDoNothing` — builds `ON CONFLICT <target> DO NOTHING`.
- `onConflictDoNothingUntargeted` — builds `ON CONFLICT DO NOTHING` (no target;
  SQLite-specific, not valid in PostgreSQL for multi-row inserts).

### Enriched module: `Orville.SQLite.Execution`

Add new entity-operations-level functions for upsert and conflict-resolution.

### Enriched module: `Orville.SQLite.SqlMarshaller`

Add `foldMarshallerFields` and supporting types (`MarshallerField`,
`ReadOnlyColumnOption`, `collectFromField`) to support iterating over the
fields in a marshaller. This is needed for `ByMarshaller` conflict target
resolution and for generating the `DO UPDATE SET` clause.

## Types

### `ConflictTarget` ADT (in `Orville.SQLite.Execution`)

```haskell
data ConflictTarget
  = ByPrimaryKey
      -- ^ Resolve conflict via the table's primary key column.
  | ByField (FieldDefinition nullability a)
      -- ^ Resolve conflict via a single field (assumed to have a UNIQUE
      --   constraint/index).
  | ByMarshaller (SqlMarshaller writeEntity readEntity)
      -- ^ Resolve conflict via all writable (non-read-only) fields in the
      --   given marshaller. Useful when the marshaller fields correspond to
      --   a multi-column unique constraint.
  | ByConflictTargetExpr ConflictTargetExpr
      -- ^ Custom conflict target expression (escape hatch).
```

### `ConflictTargetError` (in `Orville.SQLite.Execution`)

```haskell
data ConflictTargetError
  = EmptyConflictTarget
      -- ^ ByMarshaller resolved to no writable fields.
  | NoPrimaryKey
      -- ^ ByPrimaryKey used with a table that has no real primary key
      --   (e.g., mkTableDefinitionWithoutKey).
```

These implement `Show` and `Exception`, thrown via `throwIO`.

## Function Signatures

### In `Orville.SQLite.Execution`

```haskell
upsertEntity ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    writeEntity ->
    OrvilleM ()
-- ^ INSERT ... ON CONFLICT <target> DO UPDATE SET ...  No return value.

upsertAndReturnEntity ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    writeEntity ->
    OrvilleM readEntity
-- ^ Same as upsertEntity, but appends RETURNING <cols> to return the
--   upserted row as seen by the database.

insertOnConflictDoNothing ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    writeEntity ->
    OrvilleM ()
-- ^ INSERT ... ON CONFLICT <target> DO NOTHING.

insertOnConflictDoNothingUntargeted ::
    TableDefinition key writeEntity readEntity ->
    writeEntity ->
    OrvilleM ()
-- ^ INSERT ... ON CONFLICT DO NOTHING (no target).
--   SQLite-specific; accepts any unique constraint violation.
```

### In `Orville.SQLite.SqlMarshaller`

```haskell
data MarshallerField writeEntity where
  Natural ::
    FieldDefinition nullability a ->
    Maybe (writeEntity -> a) ->
    MarshallerField writeEntity

data ReadOnlyColumnOption
  = IncludeReadOnlyColumns
  | ExcludeReadOnlyColumns

collectFromField ::
  ReadOnlyColumnOption ->
  (forall n a. FieldDefinition n a -> result) ->
  MarshallerField writeEntity ->
  [result] ->
  [result]

foldMarshallerFields ::
  SqlMarshaller writeEntity readEntity ->
  acc ->
  (forall w. MarshallerField w -> acc -> acc) ->
  acc
```

## SQL Generation

### `upsertEntity` with `ByPrimaryKey`

For table `person` with PK column `id` and writable columns `first_name`,
`last_name`, `age`:

```sql
INSERT INTO person (first_name, last_name, age)
VALUES (?, ?, ?)
ON CONFLICT (id) DO UPDATE SET
    first_name = excluded.first_name,
    last_name = excluded.last_name,
    age = excluded.age
```

- Only non-read-only columns appear in the `SET` clause.
- `excluded.` pseudo-table reference follows PostgreSQL convention and works
  identically in SQLite.
- The conflict target is the PK column name.

### `upsertAndReturnEntity`

Same SQL as above, with `RETURNING <all-columns>` appended:

```sql
... ON CONFLICT (id) DO UPDATE SET ... RETURNING id, first_name, last_name, age
```

### `insertOnConflictDoNothing`

```sql
INSERT INTO person (first_name, last_name, age)
VALUES (?, ?, ?)
ON CONFLICT (id) DO NOTHING
```

### `insertOnConflictDoNothingUntargeted`

```sql
INSERT INTO person (first_name, last_name, age)
VALUES (?, ?, ?)
ON CONFLICT DO NOTHING
```

## `ConflictTarget` Resolution

```haskell
conflictTargetToConflictTargetExpr ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    Either ConflictTargetError ConflictTargetExpr
```

- **`ByPrimaryKey`**: Extracts the column name from `tablePrimaryKey`'s
  `FieldDefinition`. Errors with `NoPrimaryKey` for keyless tables.
- **`ByField fieldDef`**: Uses `fieldColumnName fieldDef`.
- **`ByMarshaller marshaller`**: Uses `foldMarshallerFields` with
  `ExcludeReadOnlyColumns` to collect column names. Errors with
  `EmptyConflictTarget` if no writable columns.
- **`ByConflictTargetExpr expr`**: Returns `expr` directly.

## SQL Clause Construction (in `Orville.SQLite.Expr.OnConflict`)

```haskell
onConflictDoUpdate ::
    ConflictTargetExpr ->
    [String] ->  -- column names for the SET clause
    OnConflictExpr

onConflictDoNothing ::
    ConflictTargetExpr ->
    OnConflictExpr

onConflictDoNothingUntargeted ::
    OnConflictExpr
```

These build `RawSql` fragments that are appended to the `INSERT` statement.

## Implementation Steps

1. **Add `foldMarshallerFields` and friends to `SqlMarshaller`** — new
   traversal infrastructure.
2. **Create `Orville.SQLite.Expr.OnConflict`** — expression types and
   construction helpers.
3. **Add `ConflictTarget`, `ConflictTargetError`, resolution logic, and new
   functions to `Execution`** — the entity-level API.
4. **Wire into `Orville.SQLite` re-exports** — expose everything from the
   top-level module.
5. **Test** — cover all API surfaces with integration tests against an
   in-memory SQLite database.

## Testing Strategy

Tests go in `test/Test/EntityOperations.hs` (extending the existing test
suite). Each test uses `withFreshDb` from `Test.Setup`.

| # | Test | Variant |
|---|------|---------|
| 1 | Upsert inserts new row (no existing conflict) | `upsertEntity` with `ByPrimaryKey` |
| 2 | Upsert updates existing row | `upsertEntity` with `ByPrimaryKey` |
| 3 | Upsert and return entity after insert | `upsertAndReturnEntity`, new row |
| 4 | Upsert and return entity after update | `upsertAndReturnEntity`, existing row |
| 5 | Do nothing skips conflict (existing row unchanged) | `insertOnConflictDoNothing` with `ByPrimaryKey` |
| 6 | Do nothing inserts new row (no conflict) | `insertOnConflictDoNothing` with `ByPrimaryKey` |
| 7 | Untargeted do nothing skips conflict | `insertOnConflictDoNothingUntargeted` |
| 8 | ByField conflict target works with unique field | `upsertEntity` with `ByField` |
| 9 | ByMarshaller conflict target works | `upsertEntity` with `ByMarshaller` |
| 10 | NoPrimaryKey error for keyless table | `upsertEntity` with `ByPrimaryKey` on `mkTableDefinitionWithoutKey` |
