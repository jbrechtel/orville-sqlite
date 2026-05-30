{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.AutoMigration where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Test.Hspec

import Orville.SQLite
import Test.Setup

autoMigrationTests :: Spec
autoMigrationTests = do
    describe "autoMigrateSchema" $ do
        it "creates a table that does not exist" $ do
            db <- openConnection ":memory:"
            withConnection db $ do
                autoMigrateSchema defaultOptions [schemaTable personTable []]
                pure ()
            closeConnection db

        it "is idempotent -- running twice does not error" $ do
            db <- openConnection ":memory:"
            withConnection db $ do
                autoMigrateSchema defaultOptions [schemaTable personTable []]
                autoMigrateSchema defaultOptions [schemaTable personTable []]
                pure ()
            closeConnection db

        it "creates multiple tables at once" $ do
            db <- openConnection ":memory:"
            withConnection db $ do
                autoMigrateSchema
                    defaultOptions
                    [ schemaTable personTable []
                    , schemaTable widgetTable []
                    ]
                pure ()
            closeConnection db

        it "adds a new nullable column via ALTER TABLE" $ do
            db <- openConnection ":memory:"
            withConnection db $ do
                autoMigrateSchema defaultOptions [existingTableSchema]
                insertEntity widgetTable (Widget 1 "Test")
                autoMigrateSchema defaultOptions [schemaTable widgetTable []]
                mWidget <- findEntity widgetTable 1
                liftIO $ mWidget `shouldBe` Just (Widget 1 "Test")
            closeConnection db

        it "attempting to use old marshaller after column drop fails" $ do
            db <- openConnection ":memory:"
            result <- liftIO $ try $ withConnection db $ do
                autoMigrateSchema defaultOptions [schemaTable personTable []]
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                autoMigrateSchema defaultOptions [schemaTable personTable ["age"]]
                -- This will fail because the marshaller still references the age column
                findEntity personTable 1
            closeConnection db
            liftIO $
                result `shouldSatisfy` \case
                    Left (_ :: SomeException) -> True
                    Right _ -> False

        it "dry run does not create tables" $ do
            db <- openConnection ":memory:"
            withConnection db $ do
                autoMigrateSchema
                    defaultOptions{runSchemaChanges = False}
                    [schemaTable personTable []]
                -- Migration is skipped; table should not exist
                result <-
                    liftIO $
                        try $
                            withConnection db $
                                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                liftIO $
                    result `shouldSatisfy` \case
                        Left (_ :: SomeException) -> True
                        Right _ -> False
            closeConnection db

-- | Schema that only has widget_id (no label), representing an older version.
existingTableSchema :: SchemaItem
existingTableSchema =
    let
        marshaller = Widget <$> marshallField widgetId widgetIdField <*> marshallReadOnlyField widgetLabelField
        tableDef = mkTableDefinition "widget" (primaryKey widgetId widgetIdField) marshaller
     in
        schemaTable tableDef []
