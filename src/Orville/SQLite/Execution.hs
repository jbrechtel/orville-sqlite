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
    ConflictTarget (..),
    ConflictTargetError (..),
    upsertEntity,
    upsertAndReturnEntity,
    insertOnConflictDoNothing,
    insertOnConflictDoNothingUntargeted,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3
import Orville.SQLite.FieldDefinition (FieldDefinition, fieldColumnName, fieldToSqlValue)
import Orville.SQLite.Expr.OnConflict (
    ConflictTargetExpr,
    OnConflictExpr (..),
    conflictTargetForColumnNames,
    onConflictDoNothing,
    onConflictDoNothingUntargeted,
    onConflictDoUpdate,
 )
import Orville.SQLite.Internal (getRowData)
import Orville.SQLite.Monad (OrvilleM)
import qualified Orville.SQLite.RawSql as RawSql
import Orville.SQLite.SqlMarshaller (
    SqlMarshaller,
    marshallerDecodeRow,
    marshallerDerivedColumns,
    marshallerEncodeWrite,
    collectFromField,
    foldMarshallerFields,
    ReadOnlyColumnOption (..),
 )
import Orville.SQLite.TableDefinition (PrimaryKey (..), TableDefinition (..))

import qualified Orville.SQLite.FieldDefinition as FieldDef

insertEntity ::
    TableDefinition key writeEntity readEntity ->
    writeEntity ->
    OrvilleM ()
insertEntity tableDef entity = do
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
                <> ")"
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs)
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt

findEntity ::
    TableDefinition key writeEntity readEntity ->
    key ->
    OrvilleM (Maybe readEntity)
findEntity tableDef key = do
    db <- ask
    let PrimaryKey _ pkFieldDef = tablePrimaryKey tableDef
    let cols = marshallerDerivedColumns (tableMarshaller tableDef)
    let sql =
            "SELECT "
                <> T.pack (intercalate ", " cols)
                <> " FROM "
                <> T.pack (tableName tableDef)
                <> " WHERE "
                <> T.pack (fieldColumnName pkFieldDef)
                <> " = ?"
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt [fieldToSqlValue key pkFieldDef]
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
    let sql =
            "SELECT "
                <> T.pack (intercalate ", " cols)
                <> " FROM "
                <> T.pack (tableName tableDef)
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
    let sql =
            "UPDATE "
                <> T.pack (tableName tableDef)
                <> " SET "
                <> T.pack (intercalate ", " setClauses)
                <> " WHERE "
                <> T.pack (fieldColumnName pkFieldDef)
                <> " = ?"
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs ++ [fieldToSqlValue pkValue pkFieldDef])
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt

deleteEntity ::
    TableDefinition key writeEntity readEntity ->
    key ->
    OrvilleM ()
deleteEntity tableDef key = do
    db <- ask
    let PrimaryKey _ pkFieldDef = tablePrimaryKey tableDef
    let sql =
            "DELETE FROM "
                <> T.pack (tableName tableDef)
                <> " WHERE "
                <> T.pack (fieldColumnName pkFieldDef)
                <> " = ?"
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt [fieldToSqlValue key pkFieldDef]
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt

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
    deriving (Show, Eq)

instance Exception ConflictTargetError

-- | Convert a 'ConflictTarget' to a 'ConflictTargetExpr', or return an error
-- if the target cannot be resolved.
conflictTargetToConflictTargetExpr ::
    TableDefinition key writeEntity readEntity ->
    ConflictTarget ->
    Either ConflictTargetError ConflictTargetExpr
conflictTargetToConflictTargetExpr tableDef conflictTarget =
    case conflictTarget of
        ByPrimaryKey ->
            if tableHasRealPrimaryKey tableDef
                then
                    let PrimaryKey _ pkFieldDef = tablePrimaryKey tableDef
                     in Right $
                            conflictTargetForColumnNames
                                [fieldColumnName pkFieldDef]
                else Left NoPrimaryKey
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
                            (\fd -> FieldDef.fieldColumnName fd)
                        )
            case colNames of
                [] -> Left EmptyConflictTarget
                _ -> Right $ conflictTargetForColumnNames colNames
        ByConflictTargetExpr expr ->
            Right expr

-- | Extract the names of writable (non-read-only) columns from a table
-- definition, for use in the SET clause of DO UPDATE.
tableWritableColumnNames ::
    TableDefinition key writeEntity readEntity ->
    [String]
tableWritableColumnNames tableDef =
    foldMarshallerFields (tableMarshaller tableDef) [] $
        collectFromField
            ExcludeReadOnlyColumns
            (\fd -> FieldDef.fieldColumnName fd)

-- | Unwrap an 'OnConflictExpr' to get its underlying 'String'.
onConflictExprToString :: OnConflictExpr -> String
{-# INLINE onConflictExprToString #-}
onConflictExprToString (OnConflictExpr r) = RawSql.unRawSql r

-- | Upsert a row: @INSERT ... ON CONFLICT <target> DO UPDATE SET ...@
-- If no row conflicts, a new row is inserted.
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
    targetExpr <- case conflictTargetToConflictTargetExpr tableDef conflictTarget of
        Left err -> liftIO $ throwIO err
        Right expr -> pure expr
    let onConflictExpr =
            onConflictDoUpdate
                targetExpr
                (tableWritableColumnNames tableDef)
    let sql =
            "INSERT INTO "
                <> T.pack (tableName tableDef)
                <> " ("
                <> T.pack (intercalate ", " colNames)
                <> ") VALUES ("
                <> T.pack (intercalate ", " placeholders)
                <> ") "
                <> T.pack (onConflictExprToString onConflictExpr)
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs)
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt

-- | Upsert a row and return the upserted entity as seen by the database.
-- @INSERT ... ON CONFLICT <target> DO UPDATE SET ... RETURNING ...@
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
    targetExpr <- case conflictTargetToConflictTargetExpr tableDef conflictTarget of
        Left err -> liftIO $ throwIO err
        Right expr -> pure expr
    let onConflictExpr =
            onConflictDoUpdate
                targetExpr
                (tableWritableColumnNames tableDef)
    let sql =
            "INSERT INTO "
                <> T.pack (tableName tableDef)
                <> " ("
                <> T.pack (intercalate ", " colNames)
                <> ") VALUES ("
                <> T.pack (intercalate ", " placeholders)
                <> ") "
                <> T.pack (onConflictExprToString onConflictExpr)
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
                    Right ent -> pure ent

-- | Insert a row, skipping if a conflict occurs.
-- @INSERT ... ON CONFLICT <target> DO NOTHING@
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
    targetExpr <- case conflictTargetToConflictTargetExpr tableDef conflictTarget of
        Left err -> liftIO $ throwIO err
        Right expr -> pure expr
    let onConflictExpr = onConflictDoNothing targetExpr
    let sql =
            "INSERT INTO "
                <> T.pack (tableName tableDef)
                <> " ("
                <> T.pack (intercalate ", " colNames)
                <> ") VALUES ("
                <> T.pack (intercalate ", " placeholders)
                <> ") "
                <> T.pack (onConflictExprToString onConflictExpr)
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs)
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt

-- | Insert a row, skipping if any unique constraint violation occurs.
-- @INSERT ... ON CONFLICT DO NOTHING@ (no target specified).
-- SQLite-specific: accepts any unique constraint violation.
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
                <> T.pack (onConflictExprToString onConflictDoNothingUntargeted)
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt (map snd pairs)
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt

