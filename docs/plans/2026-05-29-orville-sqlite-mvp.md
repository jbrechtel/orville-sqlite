# orville-sqlite MVP Implementation Plan

**Goal:** Build a type-safe SQLite API with compile-time schema guarantees, modeled after Flipstone Orville PostgreSQL.

**Architecture:** GADT-based `SqlMarshaller` maps Haskell records to SQLite tables via `FieldDefinition`s. A simple reader monad (`OrvilleM`) wraps `direct-sqlite`'s `Database` handle. `AutoMigration` compares expected schema against `PRAGMA table_info` and generates DDL. Execution layer provides `insertEntity`, `findEntity`, `updateEntity`, `deleteEntity`.

**Tech Stack:** GHC 9.10.3, Stack (LTS 24.43), direct-sqlite, text, bytestring

---

## File Structure

| File | Responsibility |
|------|---------------|
| `stack.yaml` | Stack project config with direct-sqlite extra-dep |
| `package.yaml` | hpack package definition (cabal is generated) |
| `src/Orville/SQLite/Monad.hs` | `OrvilleM` reader monad, `withConnection`, `openConnection`, `closeConnection` |
| `src/Orville/SQLite/RawSql.hs` | Minimal `RawSql` newtype + `Semigroup`/`Monoid` instances |
| `src/Orville/SQLite/SqlType.hs` | `SqlType` with encode/decode for SQLite storage classes |
| `src/Orville/SQLite/FieldDefinition.hs` | GADT with `NotNull`/`Nullable`, construction functions |
| `src/Orville/SQLite/SqlMarshaller.hs` | GADT marshaller + `marshallField`, `marshallReadOnlyField`, `marshallMaybe` |
| `src/Orville/SQLite/TableDefinition.hs` | `PrimaryKey`, `TableDefinition`, `mkTableDefinition` |
| `src/Orville/SQLite/AutoMigration.hs` | `autoMigrateSchema`, `SchemaItem`, schema comparison, DDL generation |
| `src/Orville/SQLite/Execution.hs` | `insertEntity`, `findEntity`, `findAll`, `updateEntity`, `deleteEntity` |
| `src/Orville/SQLite.hs` | Top-level re-exports module |
| `test/Main.hs` | Test runner entry point |

---

## Task 1: Project Setup

**Files:**
- Create: `stack.yaml`
- Create: `package.yaml`

- [ ] **Step 1: Create stack.yaml**

Write `/work/personal/orville-sqlite/stack.yaml`:

```yaml
resolver: lts-24.43

packages:
- .

extra-deps:
- direct-sqlite-2.3.29

nix:
  enable: false
```

- [ ] **Step 2: Create package.yaml**

Write `/work/personal/orville-sqlite/package.yaml`:

```yaml
name: orville-sqlite
version: 0.1.0.0
synopsis: A type-safe SQLite API modeled after Flipstone Orville
description: >
  Maps Haskell data types to SQLite tables with compile-time schema guarantees.
  Provides type-safe column mapping, automatic schema migration, and basic CRUD
  query execution.
category: database, sqlite
author: ""
maintainer: ""
license: MIT
license-file: LICENSE
github: ""

extra-source-files:
- README.md

dependencies:
- base >=4.17 && <5
- direct-sqlite
- text
- bytestring

library:
  source-dirs: src
  exposed-modules:
  - Orville.SQLite
  - Orville.SQLite.Monad
  - Orville.SQLite.RawSql
  - Orville.SQLite.SqlType
  - Orville.SQLite.FieldDefinition
  - Orville.SQLite.SqlMarshaller
  - Orville.SQLite.TableDefinition
  - Orville.SQLite.AutoMigration
  - Orville.SQLite.Execution

tests:
  spec:
    main: Main.hs
    source-dirs: test
    dependencies:
    - orville-sqlite
    - hspec
```

- [ ] **Step 3: Run hpack to generate .cabal and verify build setup**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs hpack
./hs stack build --dry-run 2>&1 | head -30
```

Expected: The dry run should show orville-sqlite library and spec test suite. If `direct-sqlite-2.3.29` isn't available, check the latest version with `./hs stack ls dependencies --filter direct-sqlite 2>&1` and update `stack.yaml` accordingly.

- [ ] **Step 4: Create LICENSE file (MIT)**

Write `/work/personal/orville-sqlite/LICENSE`:

```
MIT License

Copyright (c) 2026

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

- [ ] **Step 5: Commit**

```bash
cd /work/personal/orville-sqlite && git add stack.yaml package.yaml LICENSE && git commit -m "Add project build configuration"
```

---

## Task 2: OrvilleM Monad + RawSql

**Files:**
- Create: `src/Orville/SQLite/Monad.hs`
- Create: `src/Orville/SQLite/RawSql.hs`

- [ ] **Step 1: Write the RawSql module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite/RawSql.hs`:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.SQLite.RawSql
  ( RawSql(..)
  , fromString
  , toRawSql
  , intercalate
  , fromText
  , space
  , comma
  , leftParen
  , rightParen
  , equals
  ) where

import Data.Monoid (Endo(..))

newtype RawSql = RawSql { unRawSql :: String }
  deriving (Show, Eq, Semigroup, Monoid)

fromString :: String -> RawSql
fromString = RawSql

toRawSql :: RawSql -> RawSql
toRawSql = id

fromText :: Text -> RawSql
fromText = RawSql . T.unpack

intercalate :: RawSql -> [RawSql] -> RawSql
intercalate sep parts = RawSql . intercalateStr (unRawSql sep) $ map unRawSql parts
  where
    intercalateStr _ [] = ""
    intercalateStr _ [x] = x
    intercalateStr s (x:xs) = x <> s <> intercalateStr s xs

space :: RawSql
space = RawSql " "

comma :: RawSql
comma = RawSql ", "

leftParen :: RawSql
leftParen = RawSql "("

rightParen :: RawSql
rightParen = RawSql ")"

equals :: RawSql
equals = RawSql " = "
```

- [ ] **Step 2: Write the OrvilleM monad module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite/Monad.hs`:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.SQLite.Monad
  ( OrvilleM
  , withConnection
  , openConnection
  , closeConnection
  , runOrvilleM
  , withTransaction
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, runReaderT, ask, MonadReader)
import qualified Database.SQLite3 as SQLite3

newtype OrvilleM a = OrvilleM
  { unOrvilleM :: ReaderT SQLite3.Database IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader SQLite3.Database)

runOrvilleM :: SQLite3.Database -> OrvilleM a -> IO a
runOrvilleM db action = runReaderT (unOrvilleM action) db

withConnection :: SQLite3.Database -> OrvilleM a -> IO a
withConnection = runOrvilleM

openConnection :: String -> IO SQLite3.Database
openConnection path = SQLite3.open (T.pack path)

closeConnection :: SQLite3.Database -> IO ()
closeConnection = SQLite3.close

withTransaction :: OrvilleM a -> OrvilleM a
withTransaction action = do
  db <- ask
  liftIO $ SQLite3.exec db "BEGIN TRANSACTION"
  result <- action
  liftIO $ SQLite3.exec db "COMMIT"
  pure result
```

- [ ] **Step 3: Verify compilation**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs hpack && ./hs stack build 2>&1
```

Expected: Successful build (only these two modules, no warnings from -Wall).

If `Database.SQLite3` module name is wrong (e.g. `Database.SQLite3.Direct`), the build will fail with "Could not find module". Fix the import and retry.

- [ ] **Step 4: Commit**

```bash
cd /work/personal/orville-sqlite && git add src/Orville/SQLite/Monad.hs src/Orville/SQLite/RawSql.hs && git commit -m "Add OrvilleM monad and RawSql modules"
```

---

## Task 3: SqlType — SQLite Type Encoding/Decoding

**Files:**
- Create: `src/Orville/SQLite/SqlType.hs`

- [ ] **Step 1: Write the SqlType module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite/SqlType.hs`:

```haskell
module Orville.SQLite.SqlType
  ( SqlType(..)
  , integerType
  , textType
  , realType
  , blobType
  , convertSqlType
  ) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Database.SQLite3 as SQLite3

data SqlType a = SqlType
  { sqlTypeName :: String
  , sqlTypeToSql :: a -> SQLite3.SQLData
  , sqlTypeFromSql :: SQLite3.SQLData -> Either String a
  }

integerType :: SqlType Int64
integerType = SqlType
  { sqlTypeName = "INTEGER"
  , sqlTypeToSql = SQLite3.SQLInteger
  , sqlTypeFromSql = \case
      SQLite3.SQLInteger i -> Right i
      SQLite3.SQLNull -> Right 0
      other -> Left $ "Expected INTEGER, got " <> show (sqlDataKind other)
  }

textType :: SqlType T.Text
textType = SqlType
  { sqlTypeName = "TEXT"
  , sqlTypeToSql = SQLite3.SQLText
  , sqlTypeFromSql = \case
      SQLite3.SQLText t -> Right t
      SQLite3.SQLNull -> Right T.empty
      SQLite3.SQLInteger i -> Right (T.pack (show i))
      SQLite3.SQLFloat d -> Right (T.pack (show d))
      other -> Left $ "Expected TEXT, got " <> show (sqlDataKind other)
  }

realType :: SqlType Double
realType = SqlType
  { sqlTypeName = "REAL"
  , sqlTypeToSql = SQLite3.SQLFloat
  , sqlTypeFromSql = \case
      SQLite3.SQLFloat d -> Right d
      SQLite3.SQLInteger i -> Right (fromIntegral i)
      SQLite3.SQLNull -> Right 0.0
      other -> Left $ "Expected REAL, got " <> show (sqlDataKind other)
  }

blobType :: SqlType BS.ByteString
blobType = SqlType
  { sqlTypeName = "BLOB"
  , sqlTypeToSql = SQLite3.SQLBlob
  , sqlTypeFromSql = \case
      SQLite3.SQLBlob b -> Right b
      SQLite3.SQLNull -> Right BS.empty
      other -> Left $ "Expected BLOB, got " <> show (sqlDataKind other)
  }

convertSqlType :: (a -> b) -> (b -> a) -> SqlType a -> SqlType b
convertSqlType to from sqlType = SqlType
  { sqlTypeName = sqlTypeName sqlType
  , sqlTypeToSql = sqlTypeToSql sqlType . from
  , sqlTypeFromSql = fmap to . sqlTypeFromSql sqlType
  }

sqlDataKind :: SQLite3.SQLData -> String
sqlDataKind = \case
  SQLite3.SQLInteger _ -> "SQLInteger"
  SQLite3.SQLFloat _   -> "SQLFloat"
  SQLite3.SQLText _    -> "SQLText"
  SQLite3.SQLBlob _    -> "SQLBlob"
  SQLite3.SQLNull      -> "SQLNull"
```

If `SQLite3.SQLData` constructors use different names (some libraries use `SQLInt64`, `SQLDouble`, etc.), the build will fail — adjust to match the actual constructors.

- [ ] **Step 2: Verify compilation**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack build 2>&1
```

Expected: Successful build.

- [ ] **Step 3: Commit**

```bash
cd /work/personal/orville-sqlite && git add src/Orville/SQLite/SqlType.hs && git commit -m "Add SqlType module for SQLite type encoding/decoding"
```

---

## Task 4: FieldDefinition — Column Definitions with Nullability

**Files:**
- Create: `src/Orville/SQLite/FieldDefinition.hs`

- [ ] **Step 1: Write the FieldDefinition module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite/FieldDefinition.hs`:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Orville.SQLite.FieldDefinition
  ( Nullability(..)
  , FieldDefinition(..)
  , integerField
  , textField
  , realField
  , blobField
  , nullableField
  , convertField
  , fieldToSqlValue
  , fieldFromSqlValue
  , fieldColumnName
  , fieldSqlTypeName
  , fieldIsNullable
  ) where

import Data.Kind (Type)
import qualified Database.SQLite3 as SQLite3
import Orville.SQLite.SqlType (SqlType, integerType, textType, realType, blobType)

data Nullability = NotNull | Nullable

data FieldDefinition (nullability :: Nullability) :: Type -> Type where
  NotNullField ::
    { notNullFieldName :: String
    , notNullFieldSqlType :: SqlType a
    } -> FieldDefinition 'NotNull a
  NullableField ::
    { nullableFieldName :: String
    , nullableFieldSqlType :: SqlType a
    } -> FieldDefinition 'Nullable a

fieldColumnName :: FieldDefinition null a -> String
fieldColumnName = \case
  NotNullField n _ -> n
  NullableField n _ -> n

fieldSqlTypeName :: FieldDefinition null a -> String
fieldSqlTypeName = \case
  NotNullField _ st -> sqlTypeName st
  NullableField _ st -> sqlTypeName st

fieldIsNullable :: FieldDefinition null a -> Bool
fieldIsNullable = \case
  NotNullField _ _ -> False
  NullableField _ _ -> True

fieldToSqlValue :: a -> FieldDefinition null a -> SQLite3.SQLData
fieldToSqlValue val = \case
  NotNullField _ st -> sqlTypeToSql st val
  NullableField _ st -> sqlTypeToSql st val

fieldFromSqlValue :: SQLite3.SQLData -> FieldDefinition null a -> Either String a
fieldFromSqlValue sqlVal = \case
  NotNullField _ st -> sqlTypeFromSql st sqlVal
  NullableField _ st -> case sqlVal of
    SQLite3.SQLNull -> Left "Unexpected NULL for nullable field"
    _ -> sqlTypeFromSql st sqlVal

integerField :: String -> FieldDefinition 'NotNull Int64
integerField name = NotNullField name integerType

textField :: String -> FieldDefinition 'NotNull T.Text
textField name = NotNullField name textType

realField :: String -> FieldDefinition 'NotNull Double
realField name = NotNullField name realType

blobField :: String -> FieldDefinition 'NotNull BS.ByteString
blobField name = NotNullField name blobType

nullableField :: FieldDefinition 'NotNull a -> FieldDefinition 'Nullable a
nullableField = \case
  NotNullField n st -> NullableField n st

convertField ::
  (a -> b) ->
  (b -> a) ->
  FieldDefinition null a ->
  FieldDefinition null b
convertField to from = \case
  NotNullField n st -> NotNullField n (convertSqlType to from st)
  NullableField n st -> NullableField n (convertSqlType to from st)
```

- [ ] **Step 2: Add missing imports to FieldDefinition**

Open `src/Orville/SQLite/FieldDefinition.hs` and add at the top with the other imports:
```haskell
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Orville.SQLite.SqlType (SqlType, integerType, textType, realType, blobType, convertSqlType, sqlTypeName)
```

- [ ] **Step 3: Verify compilation**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack build 2>&1
```

Expected: Successful build.

- [ ] **Step 4: Commit**

```bash
cd /work/personal/orville-sqlite && git add src/Orville/SQLite/FieldDefinition.hs && git commit -m "Add FieldDefinition module with nullability type safety"
```

---

## Task 5: SqlMarshaller — GADT Record-to-Table Mapping

**Files:**
- Create: `src/Orville/SQLite/SqlMarshaller.hs`

- [ ] **Step 1: Write the SqlMarshaller module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite/SqlMarshaller.hs`:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Orville.SQLite.SqlMarshaller
  ( SqlMarshaller
  , marshallField
  , marshallReadOnlyField
  , marshallMaybe
  , marshallerDerivedColumns
  , marshallerEncodeWrite
  , marshallerDecodeRow
  , marshallerFieldDefinitions
  ) where

import Control.Applicative (liftA2)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3
import Orville.SQLite.FieldDefinition
  ( FieldDefinition(..)
  , Nullability(..)
  , fieldColumnName
  , fieldToSqlValue
  , fieldFromSqlValue
  , fieldIsNullable
  , fieldSqlTypeName
  )

data SqlMarshaller writeEntity readEntity where
  MarshallPure :: readEntity -> SqlMarshaller writeEntity readEntity
  MarshallApply ::
    SqlMarshaller writeEntity (a -> b) ->
    SqlMarshaller writeEntity a ->
    SqlMarshaller writeEntity b
  MarshallNest ::
    (writeEntity -> a) ->
    SqlMarshaller a readEntity ->
    SqlMarshaller writeEntity readEntity
  MarshallField ::
    {-# UNPACK #-} !(FieldDefinition nullability a) ->
    SqlMarshaller a a
  MarshallReadOnly ::
    SqlMarshaller a readEntity ->
    SqlMarshaller b readEntity

instance Functor (SqlMarshaller w) where
  fmap f m = MarshallPure f `MarshallApply` m

instance Applicative (SqlMarshaller w) where
  pure = MarshallPure
  (<*>) = MarshallApply

marshallField ::
  (writeEntity -> a) ->
  FieldDefinition 'NotNull a ->
  SqlMarshaller writeEntity a
marshallField accessor fieldDef = MarshallNest accessor (MarshallField fieldDef)

marshallReadOnlyField ::
  FieldDefinition nullability a ->
  SqlMarshaller writeEntity a
marshallReadOnlyField fieldDef = MarshallReadOnly (MarshallField fieldDef)

marshallMaybe ::
  FieldDefinition 'Nullable a ->
  SqlMarshaller writeEntity (Maybe a)
marshallMaybe fieldDef@(NullableField name sqlType) =
  MarshallNest (const ()) (MarshallField fieldDef) `withDecoder` decodeMaybe
  where
    decodeMaybe :: a -> Maybe a
    decodeMaybe = Just

marshallerDerivedColumns ::
  SqlMarshaller writeEntity readEntity ->
  [String]
marshallerDerivedColumns marshaller =
  reverse $ collect marshaller
  where
    collect :: SqlMarshaller w r -> [String] -> [String]
    collect (MarshallPure _) acc = acc
    collect (MarshallApply m1 m2) acc = collect m1 (collect m2 acc)
    collect (MarshallNest _ m) acc = collect m acc
    collect (MarshallField fieldDef) acc = fieldColumnName fieldDef : acc
    collect (MarshallReadOnly m) acc = collect m acc

marshallerEncodeWrite ::
  SqlMarshaller writeEntity readEntity ->
  writeEntity ->
  [(String, SQLite3.SQLData)]
marshallerEncodeWrite marshaller entity =
  reverse $ go marshaller entity []
  where
    go :: SqlMarshaller w r -> w -> [(String, SQLite3.SQLData)] -> [(String, SQLite3.SQLData)]
    go (MarshallPure _) _ acc = acc
    go (MarshallApply m1 m2) w acc =
      go m1 w (go m2 w acc)
    go (MarshallNest accessor m) w acc =
      go m (accessor w) acc
    go (MarshallField fieldDef) a acc =
      (fieldColumnName fieldDef, fieldToSqlValue a fieldDef) : acc
    go (MarshallReadOnly _) _ acc = acc

marshallerDecodeRow ::
  SqlMarshaller writeEntity readEntity ->
  [(String, SQLite3.SQLData)] ->
  Either String readEntity
marshallerDecodeRow marshaller rowData =
  go marshaller rowData
  where
    go :: SqlMarshaller w r -> [(String, SQLite3.SQLData)] -> Either String r
    go (MarshallPure r) _ = Right r
    go (MarshallApply m1 m2) rd = do
      f <- go m1 rd
      a <- go m2 rd
      Right (f a)
    go (MarshallNest _ m) rd = go m rd
    go (MarshallField fieldDef) rd =
      case lookup (fieldColumnName fieldDef) rd of
        Just sqlVal -> fieldFromSqlValue sqlVal fieldDef
        Nothing -> Left $
          "Column '" <> fieldColumnName fieldDef <> "' not found in result row"
    go (MarshallReadOnly m) rd = go m rd

marshallerFieldDefinitions ::
  SqlMarshaller writeEntity readEntity ->
  [(FieldDefinition nullability a -> r) -> r]
marshallerFieldDefinitions marshaller =
  reverse $ collect marshaller []
  where
    collect :: SqlMarshaller w r -> [forall r. (forall n a. FieldDefinition n a -> r) -> r] -> [forall r. (forall n a. FieldDefinition n a -> r) -> r]
    collect (MarshallPure _) acc = acc
    collect (MarshallApply m1 m2) acc = collect m1 (collect m2 acc)
    collect (MarshallNest _ m) acc = collect m acc
    collect (MarshallField fieldDef) acc = (\k -> k fieldDef) : acc
    collect (MarshallReadOnly m) acc = collect m acc
```

- [ ] **Step 2: Verify compilation**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack build 2>&1
```

Expected: Successful build. If `RankNTypes` approach for `marshallerFieldDefinitions` fails, simplify to just return a list of field names and types for migration:

```haskell
data FieldInfo = FieldInfo
  { fieldInfoName :: String
  , fieldInfoType :: String
  , fieldInfoIsNullable :: Bool
  }

marshallerFieldInfo ::
  SqlMarshaller writeEntity readEntity ->
  [FieldInfo]
marshallerFieldInfo marshaller =
  reverse $ go marshaller []
  where
    go (MarshallPure _) acc = acc
    go (MarshallApply m1 m2) acc = go m1 (go m2 acc)
    go (MarshallNest _ m) acc = go m acc
    go (MarshallField fieldDef) acc =
      FieldInfo
        (fieldColumnName fieldDef)
        (fieldSqlTypeName fieldDef)
        (fieldIsNullable fieldDef) : acc
    go (MarshallReadOnly m) acc = go m acc
```

- [ ] **Step 3: Commit**

```bash
cd /work/personal/orville-sqlite && git add src/Orville/SQLite/SqlMarshaller.hs && git commit -m "Add SqlMarshaller GADT with applicative combinators"
```

---

## Task 6: TableDefinition — PrimaryKey and Table Construction

**Files:**
- Create: `src/Orville/SQLite/TableDefinition.hs`

- [ ] **Step 1: Write the TableDefinition module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite/TableDefinition.hs`:

```haskell
{-# LANGUAGE GADTs #-}

module Orville.SQLite.TableDefinition
  ( PrimaryKey(..)
  , primaryKey
  , TableDefinition(..)
  , mkTableDefinition
  , mkTableDefinitionWithoutKey
  ) where

import Orville.SQLite.FieldDefinition (FieldDefinition(..), Nullability(..), fieldColumnName, fieldSqlTypeName)
import Orville.SQLite.SqlMarshaller (SqlMarshaller)

data PrimaryKey writeEntity key where
  PrimaryKey ::
    (writeEntity -> key) ->
    FieldDefinition 'NotNull key ->
    PrimaryKey writeEntity key

primaryKey ::
  (writeEntity -> key) ->
  FieldDefinition 'NotNull key ->
  PrimaryKey writeEntity key
primaryKey = PrimaryKey

data TableDefinition key writeEntity readEntity = TableDefinition
  { tableName :: String
  , tablePrimaryKey :: PrimaryKey writeEntity key
  , tableMarshaller :: SqlMarshaller writeEntity readEntity
  }

mkTableDefinition ::
  String ->
  PrimaryKey writeEntity key ->
  SqlMarshaller writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
mkTableDefinition = TableDefinition

mkTableDefinitionWithoutKey ::
  String ->
  SqlMarshaller writeEntity readEntity ->
  TableDefinition () writeEntity readEntity
mkTableDefinitionWithoutKey name marshaller = TableDefinition
  { tableName = name
  , tablePrimaryKey = PrimaryKey (const ()) (NotNullField "__no_key__" undefined)  -- TODO: improve dummy key
  , tableMarshaller = marshaller
  }
```

**Note:** `mkTableDefinitionWithoutKey` with the dummy key is a known limitation. A proper fix would use a type-level distinction between keyed and keyless tables, but that's deferred past MVP. Since MVP focuses on keyed tables, this is acceptable.

- [ ] **Step 2: Verify compilation**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack build 2>&1
```

Expected: Successful build.

- [ ] **Step 3: Commit**

```bash
cd /work/personal/orville-sqlite && git add src/Orville/SQLite/TableDefinition.hs && git commit -m "Add TableDefinition with PrimaryKey"
```

---

## Task 7: AutoMigration — Schema Comparison and DDL Generation

**Files:**
- Create: `src/Orville/SQLite/AutoMigration.hs`

- [ ] **Step 1: Write the AutoMigration module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite/AutoMigration.hs`:

```haskell
module Orville.SQLite.AutoMigration
  ( MigrationOptions(..)
  , defaultOptions
  , SchemaItem(..)
  , SchemaTable(..)
  , schemaTable
  , dropColumns
  , autoMigrateSchema
  , MigrationStep(..)
  , generateMigrationPlan
  , executeMigrationPlan
  ) where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (find, (\\))
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3
import Orville.SQLite.FieldDefinition (FieldDefinition(..), Nullability(..), fieldColumnName, fieldSqlTypeName, fieldIsNullable)
import Orville.SQLite.SqlMarshaller (marshallerFieldDefinitions, marshallerDerivedColumns, marshallerFieldInfo)
import Orville.SQLite.SqlMarshaller (SqlMarshaller, marshallerDerivedColumns)
import Orville.SQLite.TableDefinition (TableDefinition(..), PrimaryKey(..))
import Orville.SQLite.Monad (OrvilleM)
```

Then continue the file:

```haskell
data MigrationOptions = MigrationOptions
  { runSchemaChanges :: Bool
  }

defaultOptions :: MigrationOptions
defaultOptions = MigrationOptions { runSchemaChanges = True }

newtype SchemaItem = SchemaItem
  { unSchemaItem :: SchemaItemRep
  }

data SchemaItemRep
  = SchemaTableItem
    { schemaTableName :: String
    , schemaMarshaller :: SqlMarshaller w r
    , schemaPkName :: String
    , schemaDropColumns :: [String]
    }

schemaTable :: TableDefinition key writeEntity readEntity -> [String] -> SchemaItem
schemaTable tableDef dropCols =
  let
    pk = tablePrimaryKey tableDef
    pkFieldDef = case pk of PrimaryKey _ fd -> fd
  in SchemaItem $ SchemaTableItem
    { schemaTableName = tableName tableDef
    , schemaMarshaller = tableMarshaller tableDef
    , schemaPkName = fieldColumnName pkFieldDef
    , schemaDropColumns = dropCols
    }

dropColumns :: [String] -> TableDefinition key w r -> [String]
dropColumns = const

data MigrationStep
  = CreateTable String [(String, String, Bool)] (String, String)
  | AddColumn String String String Bool
  | DropColumn String String
  deriving (Show, Eq)

data ExistingColumn = ExistingColumn
  { existingName :: String
  , existingType :: String
  , existingNotNull :: Bool
  , existingPk :: Bool
  } deriving (Show, Eq)

generateMigrationPlan :: [SchemaItem] -> OrvilleM [MigrationStep]
generateMigrationPlan items = do
  steps <- concat <$> mapM planItem items
  pure steps

planItem :: SchemaItem -> OrvilleM [MigrationStep]
planItem (SchemaItem (SchemaTableItem tableName marshaller pkName dropCols)) = do
  existingCols <- getExistingColumns tableName
  let expectedCols = marshallerExpectedColumns marshaller pkName
  pure $ planTableChanges tableName expectedCols existingCols dropCols

getExistingColumns :: String -> OrvilleM [ExistingColumn]
getExistingColumns tableName = do
  db <- ask
  liftIO $ do
    stmt <- SQLite3.prepare db ("PRAGMA table_info(" <> T.pack tableName <> ")")
    let loop acc = do
          stepResult <- SQLite3.step stmt
          case stepResult of
            SQLite3.Row -> do
              -- PRAGMA table_info columns: cid, name, type, notnull, dflt_value, pk
              name <- SQLite3.columnText stmt 1
              colType <- SQLite3.columnText stmt 2
              notNull <- SQLite3.columnInt stmt 3
              isPk <- SQLite3.columnInt stmt 4
              loop (ExistingColumn (T.unpack name) (T.unpack colType) (notNull /= 0) (isPk /= 0) : acc)
            SQLite3.Done -> do
              SQLite3.finalize stmt
              pure (reverse acc)
    loop []

marshallerExpectedColumns ::
  SqlMarshaller w r ->
  String -> -- pk name
  [(String, String, Bool)] -- (colName, colType, isNullable)
marshallerExpectedColumns marshaller pkName =
  let fields = marshallerFieldInfo marshaller
  in [(fieldInfoName f, fieldInfoType f, not (fieldInfoName f == pkName || not (fieldInfoIsNullable f))) | f <- fields]

planTableChanges ::
  String ->
  [(String, String, Bool)] ->
  [ExistingColumn] ->
  [String] -> -- columns to drop
  [MigrationStep]
planTableChanges tableName expected existing dropCols
  | null existing = [CreateTable tableName expected (findPk existing)]
  | otherwise = addColumns ++ dropColumns
  where
    existingNames = map existingName existing
    expectedNames = map (\(n,_,_) -> n) expected

    -- Columns to add: in expected but not in existing
    addColumns = do
      (name, colType, _) <- expected
      if name `notElem` existingNames
      then [AddColumn tableName name colType True]  -- added columns must be nullable
      else []

    -- Columns to drop: explicitly listed columns that exist
    dropColumns = do
      name <- dropCols
      if name `elem` existingNames
      then [DropColumn tableName name]
      else []

    -- Check for type / nullability mismatches (warn but don't error for MVP)
    findPk cols =
      case find existingPk cols of
        Just col -> (existingName col, existingType col)
        Nothing -> ("", "")

autoMigrateSchema :: MigrationOptions -> [SchemaItem] -> OrvilleM ()
autoMigrateSchema opts items = do
  plan <- generateMigrationPlan items
  when (runSchemaChanges opts) $
    executeMigrationPlan plan

executeMigrationPlan :: [MigrationStep] -> OrvilleM ()
executeMigrationPlan = mapM_ executeStep
  where
    executeStep step = do
      db <- ask
      case step of
        CreateTable name cols (pkName, _) ->
          liftIO $ SQLite3.exec db (mkCreateTable name cols pkName)
        AddColumn name colName colType _ ->
          liftIO $ SQLite3.exec db ("ALTER TABLE " <> T.pack name <> " ADD COLUMN " <> T.pack colName <> " " <> T.pack colType)
        DropColumn name colName ->
          liftIO $ SQLite3.exec db ("ALTER TABLE " <> T.pack name <> " DROP COLUMN " <> T.pack colName)

mkCreateTable :: String -> [(String, String, Bool)] -> String -> T.Text
mkCreateTable name cols pkName =
  T.pack $ "CREATE TABLE IF NOT EXISTS " <> name <> " (\n  " <>
    intercalate ",\n  " (map mkColumnDef cols) <>
    ",\n  PRIMARY KEY (" <> pkName <> ")\n)"
  where
    mkColumnDef (colName, colType, isNullable) =
      colName <> " " <> colType <> if not isNullable then " NOT NULL" else ""
```

**Known issues to fix during build:**
- `marshallerFieldInfo` is defined in SqlMarshaller — need to export it
- `ask` needs `MonadReader` import
- `intercalate` needs to be `Data.List.intercalate` or a local definition
- The `generateMigrationPlan` return type doesn't need `OrvilleM` — could be pure. But accessing the database to query `PRAGMA table_info` requires `OrvilleM`, so the type is correct.

- [ ] **Step 2: Fix SqlMarshaller exports**

Edit `src/Orville/SQLite/SqlMarshaller.hs` to add `marshallerFieldInfo` and `FieldInfo(..)` to exports:
Add to the module export list:
```haskell
  , FieldInfo(..)
  , marshallerFieldInfo
```
Add the `FieldInfo` type definition:
```haskell
data FieldInfo = FieldInfo
  { fieldInfoName :: String
  , fieldInfoType :: String
  , fieldInfoIsNullable :: Bool
  }
```

- [ ] **Step 3: Fix AutoMigration imports**

Review and add missing imports to `src/Orville/SQLite/AutoMigration.hs`:
```haskell
import Control.Monad.Reader (ask)
import Orville.SQLite.Monad (OrvilleM)
import qualified Data.Text as T
```

- [ ] **Step 4: Add `intercalate` helper to AutoMigration**

Add before `mkCreateTable`:
```haskell
import Data.List (intercalate)
```

- [ ] **Step 5: Verify compilation**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack build 2>&1
```

Expected: Compilation errors related to `direct-sqlite` API differences (module names, type names). Fix each error:
- If `Database.SQLite3` doesn't export `exec` the way expected: look up the module with `./hs stack ghci -e ':browse Database.SQLite3'`
- If `SQLite3.step` returns `Either Error StepResult`: adjust pattern matches
- If column accessor names differ (`columnText` vs `column`): fix to match

- [ ] **Step 6: Commit**

```bash
cd /work/personal/orville-sqlite && git add src/Orville/SQLite/AutoMigration.hs src/Orville/SQLite/SqlMarshaller.hs && git commit -m "Add AutoMigration with schema comparison and DDL generation"
```

---

## Task 8: Execution — CRUD Operations

**Files:**
- Create: `src/Orville/SQLite/Execution.hs`

- [ ] **Step 1: Write the Execution module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite/Execution.hs`:

```haskell
module Orville.SQLite.Execution
  ( insertEntity
  , findEntity
  , findAll
  , updateEntity
  , deleteEntity
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3
import Orville.SQLite.FieldDefinition (fieldColumnName, fieldToSqlValue, fieldFromSqlValue)
import Orville.SQLite.SqlMarshaller
  ( SqlMarshaller
  , marshallerDerivedColumns
  , marshallerEncodeWrite
  , marshallerDecodeRow
  )
import Orville.SQLite.TableDefinition (TableDefinition(..), PrimaryKey(..))
import Orville.SQLite.Monad (OrvilleM)

insertEntity ::
  TableDefinition key writeEntity readEntity ->
  writeEntity ->
  OrvilleM ()
insertEntity tableDef entity = do
  db <- ask
  let pairs = marshallerEncodeWrite (tableMarshaller tableDef) entity
  let colNames = map fst pairs
  let placeholders = map (const "?") colNames
  let sql = T.pack $
        "INSERT INTO " <> tableName tableDef <>
        " (" <> intercalate ", " colNames <> ") VALUES (" <>
        intercalate ", " placeholders <> ")"
  liftIO $ do
    stmt <- SQLite3.prepare db sql
    forM_ (zip [1..] pairs) $ \(i, (_, sqlVal)) ->
      SQLite3.bind stmt i sqlVal
    _ <- SQLite3.step stmt
    SQLite3.finalize stmt

findEntity ::
  TableDefinition key writeEntity readEntity ->
  key ->
  OrvilleM (Maybe readEntity)
findEntity tableDef key = do
  db <- ask
  let pkFieldDef = case tablePrimaryKey tableDef of
        PrimaryKey _ fd -> fd
  let cols = marshallerDerivedColumns (tableMarshaller tableDef)
  let sql = T.pack $
        "SELECT " <> intercalate ", " cols <>
        " FROM " <> tableName tableDef <>
        " WHERE " <> fieldColumnName pkFieldDef <> " = ?"
  liftIO $ do
    stmt <- SQLite3.prepare db sql
    SQLite3.bind stmt 1 (fieldToSqlValue key pkFieldDef)
    stepResult <- SQLite3.step stmt
    case stepResult of
      SQLite3.Done -> do
        SQLite3.finalize stmt
        pure Nothing
      SQLite3.Row -> do
        rowData <- getRowData stmt cols
        SQLite3.finalize stmt
        case marshallerDecodeRow (tableMarshaller tableDef) rowData of
          Left err -> error $ "Decode error in findEntity: " <> err
          Right entity -> pure (Just entity)

findAll ::
  TableDefinition key writeEntity readEntity ->
  OrvilleM [readEntity]
findAll tableDef = do
  db <- ask
  let cols = marshallerDerivedColumns (tableMarshaller tableDef)
  let sql = T.pack $
        "SELECT " <> intercalate ", " cols <>
        " FROM " <> tableName tableDef
  liftIO $ do
    stmt <- SQLite3.prepare db sql
    let loop acc = do
          stepResult <- SQLite3.step stmt
          case stepResult of
            SQLite3.Done -> do
              SQLite3.finalize stmt
              pure (reverse acc)
            SQLite3.Row -> do
              rowData <- getRowData stmt cols
              case marshallerDecodeRow (tableMarshaller tableDef) rowData of
                Left err -> error $ "Decode error in findAll: " <> err
                Right entity -> loop (entity : acc)
    loop []

updateEntity ::
  TableDefinition key writeEntity readEntity ->
  writeEntity ->
  OrvilleM ()
updateEntity tableDef entity = do
  db <- ask
  let pairs = marshallerEncodeWrite (tableMarshaller tableDef) entity
  let PrimaryKey pkAccessor pkFieldDef = tablePrimaryKey tableDef
  let pkValue = pkAccessor entity
  let setClauses = map (\(col, _) -> col <> " = ?") pairs
  let sql = T.pack $
        "UPDATE " <> tableName tableDef <>
        " SET " <> intercalate ", " setClauses <>
        " WHERE " <> fieldColumnName pkFieldDef <> " = ?"
  liftIO $ do
    stmt <- SQLite3.prepare db sql
    forM_ (zip [1..] pairs) $ \(i, (_, sqlVal)) ->
      SQLite3.bind stmt i sqlVal
    let pkIndex = length pairs + 1
    SQLite3.bind stmt pkIndex (fieldToSqlValue pkValue pkFieldDef)
    _ <- SQLite3.step stmt
    SQLite3.finalize stmt

deleteEntity ::
  TableDefinition key writeEntity readEntity ->
  key ->
  OrvilleM ()
deleteEntity tableDef key = do
  db <- ask
  let pkFieldDef = case tablePrimaryKey tableDef of
        PrimaryKey _ fd -> fd
  let sql = T.pack $
        "DELETE FROM " <> tableName tableDef <>
        " WHERE " <> fieldColumnName pkFieldDef <> " = ?"
  liftIO $ do
    stmt <- SQLite3.prepare db sql
    SQLite3.bind stmt 1 (fieldToSqlValue key pkFieldDef)
    _ <- SQLite3.step stmt
    SQLite3.finalize stmt

getRowData ::
  SQLite3.Statement ->
  [String] ->
  IO [(String, SQLite3.SQLData)]
getRowData stmt cols = do
  columnCount <- SQLite3.columnCount stmt
  let indexes = [0 .. columnCount - 1]
  mapM (\i -> do
    colName <- if i < length cols
               then pure (cols !! i)
               else SQLite3.columnName stmt i
    sqlVal <- SQLite3.column stmt i
    pure (T.unpack colName, sqlVal)
    ) indexes

intercalate :: String -> [String] -> String
intercalate sep = \case
  [] -> ""
  [x] -> x
  (x:xs) -> x <> sep <> intercalate sep xs
```

- [ ] **Step 2: Verify compilation**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack build 2>&1
```

Expected: Build errors from `direct-sqlite` API mismatch. Fixes needed:
- `SQLite3.prepare` argument order or return type
- `SQLite3.bind` expects index + value or just value
- `SQLite3.step` return type (`Row`/`Done` vs `Either Error StepResult`)
- `SQLite3.column` return type
- `SQLite3.columnCount` availability
- `SQLite3.columnName` availability

Iterate on compilation until clean.

- [ ] **Step 3: Commit**

```bash
cd /work/personal/orville-sqlite && git add src/Orville/SQLite/Execution.hs && git commit -m "Add Execution module with insertEntity, findEntity, findAll, updateEntity, deleteEntity"
```

---

## Task 9: Top-Level Module Re-exports

**Files:**
- Create: `src/Orville/SQLite.hs`

- [ ] **Step 1: Write the top-level module**

Write `/work/personal/orville-sqlite/src/Orville/SQLite.hs`:

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
  ) where

import Orville.SQLite.Monad
import Orville.SQLite.SqlType
import Orville.SQLite.FieldDefinition
import Orville.SQLite.SqlMarshaller
import Orville.SQLite.TableDefinition
import Orville.SQLite.AutoMigration
import Orville.SQLite.Execution
```

- [ ] **Step 2: Fix re-export conflicts**

If any module re-exports clash (e.g., both export `intercalate`), either:
- Remove `intercalate` from sub-module exports
- Or qualify the re-export with `hiding`

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack build 2>&1
```

Fix any "duplicate export" or "ambiguous re-export" warnings.

- [ ] **Step 3: Commit**

```bash
cd /work/personal/orville-sqlite && git add src/Orville/SQLite.hs && git commit -m "Add top-level Orville.SQLite re-export module"
```

---

## Task 10: Write and Run Acceptance Tests

**Files:**
- Create: `test/Main.hs`

- [ ] **Step 1: Write the test file**

Write `/work/personal/orville-sqlite/test/Main.hs`:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Orville.SQLite

data Person = Person
  { personId :: Int64
  , firstName :: Text
  , lastName :: Text
  , age :: Int64
  } deriving (Show, Eq)

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
main = hspec $ do
  describe "Orville.SQLite" $ do
    it "creates a table and inserts a row" $ do
      db <- openConnection ":memory:"
      withConnection db $ do
        autoMigrateSchema defaultOptions [schemaTable personTable []]
        insertEntity personTable (Person 0 "Alice" "Smith" 30)
        mAlice <- findEntity personTable 1
        liftIO $ mAlice `shouldBe`
          Just (Person 1 "Alice" "Smith" 30)
      closeConnection db

    it "finds all rows" $ do
      db <- openConnection ":memory:"
      withConnection db $ do
        autoMigrateSchema defaultOptions [schemaTable personTable []]
        insertEntity personTable (Person 0 "Alice" "Smith" 30)
        insertEntity personTable (Person 0 "Bob"   "Jones"  25)
        results <- findAll personTable
        liftIO $ length results `shouldBe` 2
      closeConnection db

    it "updates a row" $ do
      db <- openConnection ":memory:"
      withConnection db $ do
        autoMigrateSchema defaultOptions [schemaTable personTable []]
        insertEntity personTable (Person 0 "Alice" "Smith" 30)
        updateEntity personTable (Person 1 "Alice" "Jones" 31)
        mAlice <- findEntity personTable 1
        liftIO $ mAlice `shouldBe`
          Just (Person 1 "Alice" "Jones" 31)
      closeConnection db

    it "deletes a row" $ do
      db <- openConnection ":memory:"
      withConnection db $ do
        autoMigrateSchema defaultOptions [schemaTable personTable []]
        insertEntity personTable (Person 0 "Alice" "Smith" 30)
        deleteEntity personTable 1
        mAlice <- findEntity personTable 1
        liftIO $ mAlice `shouldBe` Nothing
      closeConnection db

    it "returns Nothing for missing row" $ do
      db <- openConnection ":memory:"
      withConnection db $ do
        autoMigrateSchema defaultOptions [schemaTable personTable []]
        mAlice <- findEntity personTable 999
        liftIO $ mAlice `shouldBe` Nothing
      closeConnection db
```

- [ ] **Step 2: Run tests**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack test 2>&1
```

Expected: Tests may fail due to `direct-sqlite` API mismatches or logic bugs. Debug each failure:

- If `openConnection ":memory:"` returns connection but prepare fails: check `direct-sqlite` module API (e.g., `Database.SQLite3` vs `Database.SQLite3.Direct`)
- If `schemaTable personTable []` has type errors: verify `schemaTable` signature accepts args correctly
- If `insertEntity` succeeds but `findEntity` returns `Nothing` for auto-increment PK: check that SQLite's `INTEGER PRIMARY KEY` actually auto-increments (SQLite auto-increments `INTEGER PRIMARY KEY` columns by default for NULL values; ensure insert binds correct value)
- If `findEntity` can't decode results: check `getRowData` column index and name mapping

Iterate until all 5 tests pass.

- [ ] **Step 3: Commit**

```bash
cd /work/personal/orville-sqlite && git add test/Main.hs && git commit -m "Add acceptance tests for CRUD operations"
```

---

## Task 11: Build Verification and Cleanup

- [ ] **Step 1: Full build with warnings**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack build --pedantic 2>&1
```

Fix all warnings (unused imports, unused bindings, etc.).

- [ ] **Step 2: Run tests one final time**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs stack test 2>&1
```

Expected: All tests pass.

- [ ] **Step 3: Format code**

Run:
```bash
cd /work/personal/orville-sqlite && ./hs fourmolu -i src/ test/ 2>&1
```

- [ ] **Step 4: Final commit**

```bash
cd /work/personal/orville-sqlite && git add -A && git commit -m "Clean up warnings and formatting"
```
