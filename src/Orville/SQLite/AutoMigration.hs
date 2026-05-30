{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Orville.SQLite.AutoMigration
  ( MigrationOptions (..)
  , defaultOptions
  , SchemaItem (..)
  , schemaTable
  , dropColumns
  , autoMigrateSchema
  , MigrationStep (..)
  , generateMigrationPlan
  , executeMigrationPlan
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.List (find, intercalate)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3
import Orville.SQLite.FieldDefinition (fieldColumnName)
import Orville.SQLite.Monad (OrvilleM)
import Orville.SQLite.SqlMarshaller
  ( FieldInfo (..)
  , SqlMarshaller
  , marshallerFieldInfo
  )
import Orville.SQLite.TableDefinition (TableDefinition (..), PrimaryKey (..))

data MigrationOptions = MigrationOptions
  { runSchemaChanges :: Bool
  }

defaultOptions :: MigrationOptions
defaultOptions = MigrationOptions{runSchemaChanges = True}

newtype SchemaItem = SchemaItem
  { unSchemaItem :: SchemaItemRep
  }

data SchemaItemRep where
  SchemaTableItem ::
    { schemaItemTableName :: String
    , schemaItemMarshaller :: SqlMarshaller w r
    , schemaItemPkName :: String
    , schemaItemDropColumns :: [String]
    } -> SchemaItemRep

schemaTable ::
  TableDefinition key writeEntity readEntity ->
  [String] ->
  SchemaItem
schemaTable tableDef dropCols =
  let PrimaryKey _ pkFieldDef = tablePrimaryKey tableDef
   in SchemaItem
        SchemaTableItem
          { schemaItemTableName = tableName tableDef
          , schemaItemMarshaller = tableMarshaller tableDef
          , schemaItemPkName = fieldColumnName pkFieldDef
          , schemaItemDropColumns = dropCols
          }

dropColumns :: [String] -> TableDefinition key w r -> [String]
dropColumns = const

data MigrationStep
  = CreateTable String [(String, String, Bool)] String
  | AddColumn String String String
  | DropColumn String String
  deriving (Show, Eq)

data ExistingColumn = ExistingColumn
  { existingName :: String
  , existingType :: String
  , existingNotNull :: Bool
  , existingPk :: Bool
  }
  deriving (Show, Eq)

generateMigrationPlan :: [SchemaItem] -> OrvilleM [MigrationStep]
generateMigrationPlan items = concat <$> mapM planItem items

planItem :: SchemaItem -> OrvilleM [MigrationStep]
planItem (SchemaItem (SchemaTableItem tableName' marshaller pkName dropColsList)) = do
  existingCols <- getExistingColumns tableName'
  let expectedCols = marshallerExpectedColumns marshaller pkName
  pure $ planTableChanges tableName' expectedCols existingCols dropColsList

getExistingColumns :: String -> OrvilleM [ExistingColumn]
getExistingColumns tableName' = do
  db <- ask
  liftIO $ do
    stmt <-
      SQLite3.prepare db ("PRAGMA table_info(" <> T.pack tableName' <> ")")
    let loop acc = do
          stepResult <- SQLite3.step stmt
          case stepResult of
            SQLite3.Row -> do
              name <- SQLite3.columnText stmt 1
              colType <- SQLite3.columnText stmt 2
              notNullVal <- SQLite3.column stmt 3
              isPkVal <- SQLite3.column stmt 5
              let notNullFlag =
                    case notNullVal of
                      SQLite3.SQLInteger n -> n /= 0
                      _ -> False
              let isPkFlag =
                    case isPkVal of
                      SQLite3.SQLInteger n -> n /= 0
                      _ -> False
              loop
                ( ExistingColumn
                    (T.unpack name)
                    (T.unpack colType)
                    notNullFlag
                    isPkFlag
                    : acc
                )
            SQLite3.Done -> do
              SQLite3.finalize stmt
              pure (reverse acc)
    loop []

marshallerExpectedColumns ::
  SqlMarshaller w r ->
  String ->
  [(String, String, Bool)]
marshallerExpectedColumns marshaller pkName =
  [ ( fieldInfoName f
    , fieldInfoType f
    , fieldInfoName f == pkName || not (fieldInfoIsNullable f)
    )
  | f <- marshallerFieldInfo marshaller
  ]

planTableChanges ::
  String ->
  [(String, String, Bool)] ->
  [ExistingColumn] ->
  [String] ->
  [MigrationStep]
planTableChanges tableName' expected existing dropColsList
  | null existing = [CreateTable tableName' expected (findPk expected)]
  | otherwise = addColSteps ++ dropColSteps
  where
    existingNames = map existingName existing

    addColSteps =
      [ AddColumn tableName' name colType
      | (name, colType, _) <- expected
      , name `notElem` existingNames
      ]

    dropColSteps =
      [ DropColumn tableName' name
      | name <- dropColsList
      , name `elem` existingNames
      ]

    findPk cols =
      case find (\(_, _, isNull) -> not isNull) cols of
        Just (name, _, _) -> name
        Nothing -> case cols of
          ((name, _, _) : _) -> name
          [] -> ""

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
        CreateTable name cols _pkName ->
          liftIO $
            SQLite3.exec db $
              mkCreateTable name cols
        AddColumn name colName colType ->
          liftIO $
            SQLite3.exec db $
              T.pack $
                "ALTER TABLE "
                  <> name
                  <> " ADD COLUMN "
                  <> colName
                  <> " "
                  <> colType
        DropColumn name colName ->
          liftIO $
            SQLite3.exec db $
              T.pack $
                "ALTER TABLE "
                  <> name
                  <> " DROP COLUMN "
                  <> colName

mkCreateTable :: String -> [(String, String, Bool)] -> T.Text
mkCreateTable name cols =
  T.pack $
    "CREATE TABLE IF NOT EXISTS "
      <> name
      <> " (\n  "
      <> intercalate ",\n  " (map mkColumnDef cols)
      <> "\n)"
  where
    mkColumnDef (colName, colType, notNullFlag) =
      colName
        <> " "
        <> colType
        <> if notNullFlag then " NOT NULL" else ""
