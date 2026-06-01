# ON CONFLICT (Upsert / Do Nothing) Implementation Plan

**Goal:** Add upsert (`INSERT ... ON CONFLICT DO UPDATE SET`) and `ON CONFLICT DO NOTHING` support to orville-sqlite, modeled after the flipstone-orville PostgreSQL library.

**Architecture:** Three changes: (1) add `foldMarshallerFields` traversal to `SqlMarshaller` for iterating writable fields, (2) create `Orville.SQLite.Expr.OnConflict` for building `ON CONFLICT` SQL clauses via `RawSql`, (3) add `ConflictTarget`, upsert, and do-nothing functions to `Execution`. Wire re-exports through `Orville.SQLite`.

**Tech Stack:** GHC 9.10.3, Stack, direct-sqlite, hspec

---
### File Structure

| File | Action | Responsibility |
|------|--------|----------------|
| `src/Orville/SQLite/SqlMarshaller.hs` | Modify | Add `foldMarshallerFields`, `MarshallerField`, `ReadOnlyColumnOption`, `collectFromField` |
| `src/Orville/SQLite/Expr/OnConflict.hs` | Create | `OnConflictExpr`, `ConflictTargetExpr`, `onConflictDoUpdate`, `onConflictDoNothing`, `onConflictDoNothingUntargeted` |
| `src/Orville/SQLite/Execution.hs` | Modify | Add `ConflictTarget`, `ConflictTargetError`, `upsertEntity`, `upsertAndReturnEntity`, `insertOnConflictDoNothing`, `insertOnConflictDoNothingUntargeted` |
| `src/Orville/SQLite.hs` | Modify | Re-export new modules and types |
| `orville-sqlite.cabal` | Modify | Register `Orville.SQLite.Expr.OnConflict` |
| `test/Test/Setup.hs` | Modify | Add `PersonUniqueEmail` fixture (table with UNIQUE on email for ByField tests) |
| `test/Test/EntityOperations.hs` | Modify | Add all upsert/do-nothing tests |

### Task 1: Add `foldMarshallerFields` to SqlMarshaller

**Files:** `src/Orville/SQLite/SqlMarshaller.hs`

- [ ] **Step 1: Add `foldMarshallerFields` and supporting types**

Add after the `marshallReadOnly` constructor and its export:

```haskell
module Orville.SQLite.SqlMarshaller (
    SqlMarshaller,
    FieldInfo (..),
    marshallField,
    marshallReadOnlyField,
    marshallMaybe,
    marshallerFieldInfo,
    marshallerDerivedColumns,
    marshallerEncodeWrite,
    marshallerDecodeRow,
    -- NEW exports
    MarshallerField (..),
    ReadOnlyColumnOption (..),
    collectFromField,
    foldMarshallerFields,
) where
```

Add these new types and functions before the existing `Functor` / `Applicative` instances (or after `marshallReadOnlyField`, but before `marshallerFieldInfo`):

```haskell
-- | Represents a primitive field entry in a 'SqlMarshaller'. Used with
-- 'foldMarshallerFields' to iterate over the fields in a marshaller.
data MarshallerField writeEntity where
    MarshallerNatural ::
        FieldDefinition nullability a ->
        Maybe (writeEntity -> a) ->
        MarshallerField writeEntity

{- | Specifies whether read-only fields should be included when using functions
such as 'collectFromField'.
-}
data ReadOnlyColumnOption
    = IncludeReadOnlyColumns
    | ExcludeReadOnlyColumns

{- | A fold function that can be used with 'foldMarshallerFields' to collect
a value calculated from a 'FieldDefinition' via the given function. The
calculated value is added to the list of values being built.

Ignores 'MarshallerReadOnly' and 'MarshallPure' entries.
-}
collectFromField ::
    ReadOnlyColumnOption ->
    (forall n a. FieldDefinition n a -> result) ->
    MarshallerField writeEntity ->
    [result] ->
    [result]
collectFromField readOnlyOption fromField entry results =
    case entry of
        MarshallerNatural fieldDef (Just _) ->
            fromField fieldDef : results
        MarshallerNatural fieldDef Nothing ->
            case readOnlyOption of
                IncludeReadOnlyColumns -> fromField fieldDef : results
                ExcludeReadOnlyColumns -> results

{- | Fold over all the 'FieldDefinition's contained within a 'SqlMarshaller'.
This can be used to collect column names, encode to SQL values, etc.
-}
foldMarshallerFields ::
    SqlMarshaller writeEntity readEntity ->
    acc ->
    (forall w. MarshallerField w -> acc -> acc) ->
    acc
foldMarshallerFields marshaller acc0 f =
    go marshaller acc0
  where
    go :: SqlMarshaller w r -> acc -> acc
    go (MarshallPure _) acc = acc
    go (MarshallApply m1 m2) acc = go m1 (go m2 acc)
    go (MarshallNest _ m) acc = go m acc
    go (MarshallField fieldDef) acc =
        f (MarshallerNatural fieldDef (Just id)) acc
    go (MarshallMaybe fieldDef) acc =
        f (MarshallerNatural fieldDef (Just id)) acc
    go (MarshallReadOnly m) acc = go m acc
```

Note: `MarshallField` gets `Just id` as the accessor because its write entity IS the field value. `MarshallMaybe` similarly. `MarshallNest` applies the accessor first. `MarshallReadOnly` fields are traversed into, but their inner fields will have `Nothing` as the accessor, so `ExcludeReadOnlyColumns` will skip them.

- [ ] **Step 2: Build to verify compilation**

```bash
./hs stack build
```
Expected: succeeds with no errors or warnings.

- [ ] **Step 3: Commit**

```bash
git add src/Orville/SQLite/SqlMarshaller.hs
git commit -m "feat: add foldMarshallerFields traversal to SqlMarshaller"
```

### Task 2: Create `Orville.SQLite.Expr.OnConflict`

**Files:**
- Create: `src/Orville/SQLite/Expr/OnConflict.hs`

- [ ] **Step 1: Write the module**

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.SQLite.Expr.OnConflict
    ( OnConflictExpr
    , ConflictTargetExpr
    , conflictTargetForColumnNames
    , onConflictDoUpdate
    , onConflictDoNothing
    , onConflictDoNothingUntargeted
    ) where

import Data.List (intercalate)
import Orville.SQLite.RawSql (RawSql)
import qualified Orville.SQLite.RawSql as RawSql

-- | Represents the SQL 'ON CONFLICT <target> <action>' clause.
newtype OnConflictExpr
    = OnConflictExpr RawSql.RawSql
    deriving (Show, Eq, Semigroup, Monoid)

-- | Represents the conflict target portion: @ON CONFLICT (<target>)@.
newtype ConflictTargetExpr
    = ConflictTargetExpr RawSql.RawSql
    deriving (Show, Eq, Semigroup, Monoid)

-- | Build a 'ConflictTargetExpr' from a list of column names.
conflictTargetForColumnNames :: [String] -> ConflictTargetExpr
conflictTargetForColumnNames colNames =
    ConflictTargetExpr $
        RawSql.leftParen
            <> RawSql.fromString (intercalate ", " colNames)
            <> RawSql.rightParen

-- | Build an 'OnConflictExpr' that performs @ON CONFLICT <target> DO UPDATE SET
-- col1 = excluded.col1, col2 = excluded.col2, ...@
onConflictDoUpdate :: ConflictTargetExpr -> [String] -> OnConflictExpr
onConflictDoUpdate targetExpr colNames =
    OnConflictExpr $
        RawSql.fromString "ON CONFLICT "
            <> RawSql.toRawSql targetExpr
            <> RawSql.fromString " DO UPDATE SET "
            <> RawSql.fromString (intercalate ", " setClauses)
  where
    setClauses =
        [ colName <> " = excluded." <> colName
        | colName <- colNames
        ]

-- | Build an 'OnConflictExpr' that performs @ON CONFLICT <target> DO NOTHING@.
onConflictDoNothing :: ConflictTargetExpr -> OnConflictExpr
onConflictDoNothing targetExpr =
    OnConflictExpr $
        RawSql.fromString "ON CONFLICT "
            <> RawSql.toRawSql targetExpr
            <> RawSql.fromString " DO NOTHING"

-- | Build an 'OnConflictExpr' that performs @ON CONFLICT DO NOTHING@ (no target).
onConflictDoNothingUntargeted :: OnConflictExpr
onConflictDoNothingUntargeted =
    OnConflictExpr $ RawSql.fromString "ON CONFLICT DO NOTHING"
```

- [ ] **Step 2: Register in cabal file**

Add `Orville.SQLite.Expr.OnConflict` to the `exposed-modules` list in `orville-sqlite.cabal`, right after `Orville.SQLite.Execution`:

```
      Orville.SQLite.Execution
      Orville.SQLite.Expr.OnConflict
```

- [ ] **Step 3: Build to verify compilation**

```bash
./hs stack build
```
Expected: succeeds.

- [ ] **Step 4: Commit**

```bash
git add src/Orville/SQLite/Expr/OnConflict.hs orville-sqlite.cabal
git commit -m "feat: add Orville.SQLite.Expr.OnConflict module"
```

### Task 3: Add upsert/do-nothing to Execution

**Files:**
- Modify: `src/Orville/SQLite/Execution.hs`

- [ ] **Step 1: Add imports and `ConflictTarget` / `ConflictTargetError` types**

Replace the existing module header:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orville.SQLite.Execution (
    insertEntity,
    findEntity,
    findAll,
    updateEntity,
    deleteEntity,
    -- NEW
    ConflictTarget (..),
    ConflictTargetError (..),
    upsertEntity,
    upsertAndReturnEntity,
    insertOnConflictDoNothing,
    insertOnConflictDoNothingUntargeted,
) where
```

Add these imports after the existing ones:

```haskell
import Control.Exception (Exception, throwIO)
import Orville.SQLite.Expr.OnConflict (
    ConflictTargetExpr,
    OnConflictExpr,
    conflictTargetForColumnNames,
    onConflictDoNothing,
    onConflictDoNothingUntargeted,
    onConflictDoUpdate,
 )
import Orville.SQLite.SqlMarshaller (
    MarshallerField (..),
    ReadOnlyColumnOption (..),
    collectFromField,
    foldMarshallerFields,
 )
```

- [ ] **Step 2: Add `ConflictTarget` and `ConflictTargetError` types**

Add after the imports, before the existing `insertEntity`:

```haskell
-- | Specifies the target for the @ON CONFLICT@ clause of an upsert or
-- do-nothing operation.
data ConflictTarget where
    {- | Upsert / do-nothing by the table's primary key column.
    May only be used with tables that have a real primary key (not
    'mkTableDefinitionWithoutKey').
    -}
    ByPrimaryKey :: ConflictTarget
    {- | Upsert / do-nothing by a single field, assuming the field has a
    UNIQUE constraint.
    -}
    ByField :: FieldDefinition nullability a -> ConflictTarget
    {- | Upsert / do-nothing by all writable (non-read-only) fields in the
    given marshaller. Useful for multi-column unique constraints.
    -}
    ByMarshaller :: SqlMarshaller writeEntity readEntity -> ConflictTarget
    {- | Upsert / do-nothing with a custom 'ConflictTargetExpr'.
    -}
    ByConflictTargetExpr :: ConflictTargetExpr -> ConflictTarget

-- | An error resulting from attempting to construct an invalid
-- 'ConflictTargetExpr'.
data ConflictTargetError
    = EmptyConflictTarget
    | NoPrimaryKey
    deriving (Show)

instance Exception ConflictTargetError
```

- [ ] **Step 3: Add conflict target resolution helper**

```haskell
conflictTargetToConflictTargetExpr ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    Either ConflictTargetError ConflictTargetExpr
conflictTargetToConflictTargetExpr tableDef conflictTarget =
    case conflictTarget of
        ByPrimaryKey ->
            let PrimaryKey _ pkFieldDef = tablePrimaryKey tableDef
             in Right $
                    conflictTargetForColumnNames [fieldColumnName pkFieldDef]
        ByField fieldDef ->
            Right $
                conflictTargetForColumnNames [fieldColumnName fieldDef]
        ByMarshaller marshaller -> do
            let colNames =
                    foldMarshallerFields
                        marshaller
                        []
                        ( collectFromField
                            ExcludeReadOnlyColumns
                            (const fieldColumnName)
                        )
            case colNames of
                [] -> Left EmptyConflictTarget
                _ -> Right $ conflictTargetForColumnNames colNames
        ByConflictTargetExpr expr ->
            Right expr
```

- [ ] **Step 4: Add writable column names extraction helper**

Needed to build the `DO UPDATE SET ... excluded.col = excluded.col` clause:

```haskell
-- | Extract the names of writable (non-read-only) columns from a table
-- definition, for use in the SET clause of DO UPDATE.
tableWritableColumnNames ::
    TableDefinition key writeEntity readEntity ->
    [String]
tableWritableColumnNames tableDef =
    foldMarshallerFields (tableMarshaller tableDef) [] $
        collectFromField
            ExcludeReadOnlyColumns
            (const fieldColumnName)
```

- [ ] **Step 5: Add `upsertEntity`**

```haskell
upsertEntity ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    writeEntity ->
    OrvilleM ()
upsertEntity tableDef conflictTarget entity = do
    db <- ask
    let pairs = marshallerEncodeWrite (tableMarshaller tableDef) entity
    let colNames = map fst pairs
    let placeholders = map (const "?") colNames
    let onConflictExpr =
            case conflictTargetToConflictTargetExpr tableDef conflictTarget of
                Left err -> throwIO err
                Right target ->
                    onConflictDoUpdate
                        target
                        (tableWritableColumnNames tableDef)
    let sql =
            "INSERT INTO "
                <> T.pack (tableName tableDef)
                <> " ("
                <> T.pack (intercalate ", " colNames)
                <> ") VALUES ("
                <> T.pack (intercalate ", " placeholders)
                <> ") "
                <> RawSql.unRawSql (unOnConflictExpr onConflictExpr)
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs)
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt
  where
    -- Unwrap the newtype; we'll import RawSql for this
    unOnConflictExpr (OnConflictExpr r) = RawSql.unRawSql r
```

Note: We need to import `RawSql` (the module, not just the type). Add this import:

```haskell
import qualified Orville.SQLite.RawSql as RawSql
```

Also need to import `OnConflictExpr` constructor — update the import from `Orville.SQLite.Expr.OnConflict`:

```haskell
import Orville.SQLite.Expr.OnConflict (
    ConflictTargetExpr,
    OnConflictExpr (..),
    conflictTargetForColumnNames,
    onConflictDoNothing,
    onConflictDoNothingUntargeted,
    onConflictDoUpdate,
 )
```

- [ ] **Step 6: Add `upsertAndReturnEntity`**

```haskell
upsertAndReturnEntity ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    writeEntity ->
    OrvilleM readEntity
upsertAndReturnEntity tableDef conflictTarget entity = do
    db <- ask
    let pairs = marshallerEncodeWrite (tableMarshaller tableDef) entity
    let colNames = map fst pairs
    let placeholders = map (const "?") colNames
    let allCols = marshallerDerivedColumns (tableMarshaller tableDef)
    let onConflictExpr =
            case conflictTargetToConflictTargetExpr tableDef conflictTarget of
                Left err -> throwIO err
                Right target ->
                    onConflictDoUpdate
                        target
                        (tableWritableColumnNames tableDef)
    let sql =
            "INSERT INTO "
                <> T.pack (tableName tableDef)
                <> " ("
                <> T.pack (intercalate ", " colNames)
                <> ") VALUES ("
                <> T.pack (intercalate ", " placeholders)
                <> ") "
                <> RawSql.unRawSql (unOnConflictExpr onConflictExpr)
                <> " RETURNING "
                <> T.pack (intercalate ", " allCols)
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs)
        stepResult <- SQLite3.step stmt
        case stepResult of
            SQLite3.Done -> do
                SQLite3.finalize stmt
                error "upsertAndReturnEntity: INSERT ... RETURNING returned no rows"
            SQLite3.Row -> do
                rowData <- getRowData stmt allCols
                SQLite3.finalize stmt
                case marshallerDecodeRow (tableMarshaller tableDef) rowData of
                    Left err -> error $ "Decode error in upsertAndReturnEntity: " <> err
                    Right entity -> pure entity
  where
    unOnConflictExpr (OnConflictExpr r) = RawSql.unRawSql r
```

- [ ] **Step 7: Add `insertOnConflictDoNothing`**

```haskell
insertOnConflictDoNothing ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    writeEntity ->
    OrvilleM ()
insertOnConflictDoNothing tableDef conflictTarget entity = do
    db <- ask
    let pairs = marshallerEncodeWrite (tableMarshaller tableDef) entity
    let colNames = map fst pairs
    let placeholders = map (const "?") colNames
    let onConflictExpr =
            case conflictTargetToConflictTargetExpr tableDef conflictTarget of
                Left err -> throwIO err
                Right target ->
                    onConflictDoNothing target
    let sql =
            "INSERT INTO "
                <> T.pack (tableName tableDef)
                <> " ("
                <> T.pack (intercalate ", " colNames)
                <> ") VALUES ("
                <> T.pack (intercalate ", " placeholders)
                <> ") "
                <> RawSql.unRawSql (unOnConflictExpr onConflictExpr)
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs)
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt
  where
    unOnConflictExpr (OnConflictExpr r) = RawSql.unRawSql r
```

- [ ] **Step 8: Add `insertOnConflictDoNothingUntargeted`**

```haskell
insertOnConflictDoNothingUntargeted ::
    TableDefinition key writeEntity readEntity ->
    writeEntity ->
    OrvilleM ()
insertOnConflictDoNothingUntargeted tableDef entity = do
    db <- ask
    let pairs = marshallerEncodeWrite (tableMarshaller tableDef) entity
    let colNames = map fst pairs
    let placeholders = map (const "?") colNames
    let sql =
            "INSERT INTO "
                <> T.pack (tableName tableDef)
                <> " ("
                <> T.pack (intercalate ", " colNames)
                <> ") VALUES ("
                <> T.pack (intercalate ", " placeholders)
                <> ") "
                <> RawSql.unRawSql (unOnConflictExpr onConflictDoNothingUntargeted)
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs)
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt
  where
    unOnConflictExpr (OnConflictExpr r) = RawSql.unRawSql r
```

- [ ] **Step 9: Build to verify compilation**

```bash
./hs stack build
```
Expected: succeeds.

- [ ] **Step 10: Commit**

```bash
git add src/Orville/SQLite/Execution.hs
git commit -m "feat: add upsert and ON CONFLICT DO NOTHING to Execution"
```

### Task 4: Wire re-exports through `Orville.SQLite`

**Files:**
- Modify: `src/Orville/SQLite.hs`

- [ ] **Step 1: Add new re-exports**

Replace the existing module body with:

```haskell
module Orville.SQLite
  ( -- * Monad
    module Orville.SQLite.Monad
    -- * SqlType
  , module Orville.SQLite.SqlType
    -- * FieldDefinition
  , module Orville.SQLite.FieldDefinition
    -- * SqlMarshaller
  , module Orville.SQLite.SqlMarshaller
    -- * TableDefinition
  , module Orville.SQLite.TableDefinition
    -- * AutoMigration
  , module Orville.SQLite.AutoMigration
    -- * Execution
  , module Orville.SQLite.Execution
    -- * Raw
  , module Orville.SQLite.Raw
    -- * Expr
  , module Orville.SQLite.Expr.OnConflict
  ) where

import Orville.SQLite.AutoMigration
import Orville.SQLite.Execution
import Orville.SQLite.Expr.OnConflict
import Orville.SQLite.FieldDefinition
import Orville.SQLite.Monad
import Orville.SQLite.Raw
import Orville.SQLite.SqlMarshaller
import Orville.SQLite.SqlType
import Orville.SQLite.TableDefinition
```

- [ ] **Step 2: Build to verify compilation**

```bash
./hs stack build
```
Expected: succeeds.

- [ ] **Step 3: Commit**

```bash
git add src/Orville/SQLite.hs
git commit -m "feat: re-export OnConflict module from Orville.SQLite"
```

### Task 5: Add test fixtures for upsert/do-nothing testing

**Files:**
- Modify: `test/Test/Setup.hs`

- [ ] **Step 1: Add unique-email fixture**

Add after the existing `Task` fixture:

```haskell
-- | An entity with a UNIQUE constraint on email (used for ByField testing).
-- The table must be created manually with a UNIQUE constraint since the
-- auto-migration doesn't support them yet.
data PersonUniqueEmail = PersonUniqueEmail
    { pueId :: Int64
    , pueName :: Text
    , pueEmail :: Text
    }
    deriving (Show, Eq)

pueIdField :: FieldDefinition 'NotNull Int64
pueIdField = integerField "id"

pueNameField :: FieldDefinition 'NotNull Text
pueNameField = textField "name"

pueEmailField :: FieldDefinition 'NotNull Text
pueEmailField = textField "email"

pueMarshaller :: SqlMarshaller PersonUniqueEmail PersonUniqueEmail
pueMarshaller =
    PersonUniqueEmail
        <$> marshallReadOnlyField pueIdField
        <*> marshallField pueName pueNameField
        <*> marshallField pueEmail pueEmailField

pueTable :: TableDefinition Int64 PersonUniqueEmail PersonUniqueEmail
pueTable =
    mkTableDefinition "person_unique_email" (primaryKey pueId pueIdField) pueMarshaller
```

- [ ] **Step 2: Add a helper for creating tables with extra SQL**

Add this helper after `withFreshDb`:

```haskell
{- | Run an OrvilleM action against a fresh in-memory database, creating the
table with custom SQL (to support UNIQUE constraints etc.) before running the
standard auto-migration.

The custom SQL is run first, so any tables it creates will be skipped by
auto-migration's 'CreateTable' step.
-}
withFreshDbExtras ::
    [T.Text] -> -- ^ extra SQL statements to run before autoMigrateSchema
    TableDefinition key w r ->
    OrvilleM a ->
    IO a
withFreshDbExtras extras tableDef action = do
    db <- openConnection ":memory:"
    result <- withConnection db $ do
        mapM_ execute extras
        autoMigrateSchema defaultOptions [schemaTable tableDef []]
        action
    closeConnection db
    pure result
```

Need to add the import:
```haskell
import qualified Data.Text as T
```

- [ ] **Step 3: Build to verify compilation**

```bash
./hs stack build
```
Expected: succeeds.

- [ ] **Step 4: Commit**

```bash
git add test/Test/Setup.hs
git commit -m "test: add PersonUniqueEmail fixture and withFreshDbExtras helper"
```

### Task 6: Add upsert/do-nothing tests

**Files:**
- Modify: `test/Test/EntityOperations.hs`

- [ ] **Step 1: Add imports**

Add these imports at the top:

```haskell
import qualified Data.Text as T
import Control.Exception (try, IOException)
import Orville.SQLite.Expr.OnConflict (ConflictTargetExpr, conflictTargetForColumnNames)
```

- [ ] **Step 2: Add the first test group — `upsertEntity` tests**

Add within the `describe "Execution"` block (or create a new top-level describe). Add after the existing `deleteEntity` tests:

```haskell
    describe "upsertEntity" $ do
        it "inserts a new row when no conflict exists" $ do
            results <- withFreshDb personTable $ do
                upsertEntity personTable ByPrimaryKey (Person 0 "Alice" "Smith" 30)
                findAll personTable
            liftIO $ length results `shouldBe` 1
            liftIO $ firstName (head results) `shouldBe` "Alice"

        it "updates an existing row on conflict by primary key" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                upsertEntity personTable ByPrimaryKey (Person 1 "Alice" "Jones" 31)
                findAll personTable
            liftIO $ length results `shouldBe` 1
            liftIO $ lastName (head results) `shouldBe` "Jones"
            liftIO $ age (head results) `shouldBe` 31

        it "upserts by a unique field via ByField" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS person_unique_email ("
                        , "id INTEGER PRIMARY KEY,"
                        , "name TEXT NOT NULL,"
                        , "email TEXT NOT NULL UNIQUE"
                        , ")"
                        ]
            results <- withFreshDbExtras [createTable] pueTable $ do
                insertEntity pueTable (PersonUniqueEmail 1 "Alice" "alice@example.com")
                -- Upsert by email field (not PK)
                upsertEntity
                    pueTable
                    (ByField pueEmailField)
                    (PersonUniqueEmail 1 "Alice Updated" "alice@example.com")
                findAll pueTable
            liftIO $ length results `shouldBe` 1
            liftIO $ pueName (head results) `shouldBe` "Alice Updated"
```

- [ ] **Step 3: Add `upsertAndReturnEntity` tests**

```haskell
    describe "upsertAndReturnEntity" $ do
        it "inserts a new row and returns the entity" $ do
            result <- withFreshDb personTable $ do
                upsertAndReturnEntity personTable ByPrimaryKey (Person 0 "Alice" "Smith" 30)
            liftIO $ result `shouldBe` Person 1 "Alice" "Smith" 30

        it "updates an existing row and returns the updated entity" $ do
            result <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                upsertAndReturnEntity personTable ByPrimaryKey (Person 1 "Alice" "Jones" 31)
            liftIO $ result `shouldBe` Person 1 "Alice" "Jones" 31
```

- [ ] **Step 4: Add `insertOnConflictDoNothing` tests**

```haskell
    describe "insertOnConflictDoNothing" $ do
        it "skips insert on conflict by primary key" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                insertOnConflictDoNothing personTable ByPrimaryKey (Person 1 "Bob" "Jones" 25)
                findAll personTable
            liftIO $ length results `shouldBe` 1
            liftIO $ firstName (head results) `shouldBe` "Alice"

        it "inserts when no conflict exists" $ do
            results <- withFreshDb personTable $ do
                insertOnConflictDoNothing personTable ByPrimaryKey (Person 0 "Alice" "Smith" 30)
                findAll personTable
            liftIO $ length results `shouldBe` 1

        it "skips insert on conflict by a unique field" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS person_unique_email ("
                        , "id INTEGER PRIMARY KEY,"
                        , "name TEXT NOT NULL,"
                        , "email TEXT NOT NULL UNIQUE"
                        , ")"
                        ]
            results <- withFreshDbExtras [createTable] pueTable $ do
                insertEntity pueTable (PersonUniqueEmail 1 "Alice" "alice@example.com")
                insertOnConflictDoNothing
                    pueTable
                    (ByField pueEmailField)
                    (PersonUniqueEmail 2 "Bob" "alice@example.com")
                findAll pueTable
            liftIO $ length results `shouldBe` 1
            liftIO $ pueName (head results) `shouldBe` "Alice"
```

- [ ] **Step 5: Add `insertOnConflictDoNothingUntargeted` test**

```haskell
    describe "insertOnConflictDoNothingUntargeted" $ do
        it "skips insert on any conflict without specifying a target" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                insertOnConflictDoNothingUntargeted personTable (Person 1 "Bob" "Jones" 25)
                findAll personTable
            liftIO $ length results `shouldBe` 1
            liftIO $ firstName (head results) `shouldBe` "Alice"

        it "inserts when no conflict exists" $ do
            results <- withFreshDb personTable $ do
                insertOnConflictDoNothingUntargeted personTable (Person 0 "Alice" "Smith" 30)
                findAll personTable
            liftIO $ length results `shouldBe` 1
```

- [ ] **Step 6: Add `ByMarshaller` test and error case tests**

```haskell
    describe "ConflictTarget resolution" $ do
        it "ByMarshaller upserts using all writable fields" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS person_unique_email ("
                        , "id INTEGER PRIMARY KEY,"
                        , "name TEXT NOT NULL,"
                        , "email TEXT NOT NULL UNIQUE"
                        , ")"
                        ]
            results <- withFreshDbExtras [createTable] pueTable $ do
                insertEntity pueTable (PersonUniqueEmail 1 "Alice" "alice@example.com")
                -- ByMarshaller uses (name, email) as conflict target
                upsertEntity
                    pueTable
                    (ByMarshaller pueMarshaller)
                    (PersonUniqueEmail 1 "Alice" "alice@example.com")
                findAll pueTable
            liftIO $ length results `shouldBe` 1

        it "ByConflictTargetExpr works with a custom expression" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                let target = conflictTargetForColumnNames ["id"]
                upsertEntity personTable (ByConflictTargetExpr target) (Person 1 "Alice" "Jones" 31)
                findAll personTable
            liftIO $ length results `shouldBe` 1
            liftIO $ lastName (head results) `shouldBe` "Jones"

        it "NoPrimaryKey error for keyless table" $ do
            let keylessDef =
                    mkTableDefinitionWithoutKey "keyless" personMarshaller
            result <- try $ withFreshDb keylessDef $
                upsertEntity keylessDef ByPrimaryKey (Person 0 "Alice" "Smith" 30)
            liftIO $ result `shouldSatisfy` \case
                Left (ConflictTargetError.NoPrimaryKey) -> True
                _ -> False
          where
            ConflictTargetError = id -- import from Execution module
```

Wait — `ConflictTargetError` constructors can't be pattern-matched with `Left` since `ConflictTargetError` isn't `IOException`. Let me fix this approach — we'll use `Control.Exception.catch` with `Handler` or just check the exception type differently. Actually the cleanest way is:

```haskell
import Control.Exception (Exception, catch)

it "NoPrimaryKey error for keyless table" $ do
    let keylessDef = mkTableDefinitionWithoutKey "keyless" personMarshaller
    caught <- withFreshDb keylessDef $
        Control.Exception.catch
            (upsertEntity keylessDef ByPrimaryKey (Person 0 "Alice" "Smith" 30) >> pure False)
            (\(_ :: ConflictTargetError) -> pure True)
    liftIO $ caught `shouldBe` True
```

And we also need `Control.Exception` already imported for `catch`. Let me use `try` but specialize it properly:

```haskell
import Control.Exception (Exception, try, throwIO)

    it "NoPrimaryKey error for keyless table" $ do
        let keylessDef = mkTableDefinitionWithoutKey "keyless" personMarshaller
        result <-
            try $
                withFreshDb keylessDef $
                    upsertEntity keylessDef ByPrimaryKey (Person 0 "Alice" "Smith" 30)
        liftIO $ result `shouldSatisfy` \case
            Left NoPrimaryKey -> True
            Right _ -> False
        liftIO $ result `shouldBe` (Left NoPrimaryKey :: Either ConflictTargetError ())
```

This works because `Exception.try :: Exception e => IO a -> IO (Either e a)` and `ConflictTargetError` is an `Exception`.

- [ ] **Step 7: Build and run tests**

```bash
./hs stack test
```
Expected: all tests pass (existing + new).

- [ ] **Step 8: Commit**

```bash
git add test/Test/EntityOperations.hs
git commit -m "test: add tests for upsert and ON CONFLICT DO NOTHING"
```
