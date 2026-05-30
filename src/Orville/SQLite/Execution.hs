{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orville.SQLite.Execution
  ( insertEntity
  , findEntity
  , findAll
  , updateEntity
  , deleteEntity
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3
import Database.SQLite3.Direct (columnCount)
import Orville.SQLite.FieldDefinition (fieldColumnName, fieldToSqlValue)
import Orville.SQLite.Monad (OrvilleM)
import Orville.SQLite.SqlMarshaller
  ( marshallerDerivedColumns
  , marshallerEncodeWrite
  , marshallerDecodeRow
  )
import Orville.SQLite.TableDefinition (TableDefinition (..), PrimaryKey (..))

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

getRowData ::
  SQLite3.Statement ->
  [String] ->
  IO [(String, SQLite3.SQLData)]
getRowData stmt cols = do
  colCount <- columnCount stmt
  let count :: Int = fromIntegral colCount
      indexes = take count [0 :: SQLite3.ColumnIndex ..]
  mapM
    ( \i -> do
        let idx :: Int = fromIntegral i
            colName =
              if idx < length cols
                then cols !! idx
                else ""
        sqlVal <- SQLite3.column stmt i
        pure (colName, sqlVal)
    )
    indexes
