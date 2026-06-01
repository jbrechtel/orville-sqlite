{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.EntityOperations where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Test.Hspec

import Orville.SQLite
import Test.Setup

entityOperationsTests :: Spec
entityOperationsTests = do
    describe "insertEntity" $ do
        it "inserts a row and returns via findEntity" $ do
            let p = Person 0 "Alice" "Smith" 30
            mAlice <- withFreshDb personTable $ do
                insertEntity personTable p
                findEntity personTable 1
            liftIO $ mAlice `shouldBe` Just (Person 1 "Alice" "Smith" 30)

        it "assigns auto-increment ids sequentially" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "A" "One" 20)
                insertEntity personTable (Person 0 "B" "Two" 25)
                insertEntity personTable (Person 0 "C" "Three" 30)
                findAll personTable
            liftIO $ map personId results `shouldBe` [1, 2, 3]

        it "inserts an entity with explicit PK (non-auto-increment)" $ do
            let w = Widget 42 "TestWidget"
            mWidget <- withFreshDb widgetTable $ do
                insertEntity widgetTable w
                findEntity widgetTable 42
            liftIO $ mWidget `shouldBe` Just w

        it "inserts an entity with a nullable field set to Just" $ do
            let t = Task 0 "Buy groceries" (Just 15)
            mTask <- withFreshDb taskTable $ do
                insertEntity taskTable t
                findEntity taskTable 1
            liftIO $ mTask `shouldBe` Just (Task 1 "Buy groceries" (Just 15))

        it "inserts an entity with a nullable field set to Nothing" $ do
            let t = Task 0 "No due date" Nothing
            mTask <- withFreshDb taskTable $ do
                insertEntity taskTable t
                findEntity taskTable 1
            liftIO $ mTask `shouldBe` Just (Task 1 "No due date" Nothing)

    describe "findEntity" $ do
        it "returns Nothing for a non-existent key" $ do
            result <-
                withFreshDb personTable $
                    findEntity personTable 999
            liftIO $ result `shouldBe` Nothing

        it "returns Nothing on an empty table" $ do
            result <-
                withFreshDb personTable $
                    findEntity personTable 1
            liftIO $ result `shouldBe` Nothing

    describe "findAll" $ do
        it "returns all inserted rows" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                insertEntity personTable (Person 0 "Bob" "Jones" 25)
                findAll personTable
            liftIO $ length results `shouldBe` 2

        it "returns empty list on empty table" $ do
            results <-
                withFreshDb personTable $
                    findAll personTable
            liftIO $ results `shouldBe` []

        it "returns rows in insertion order" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "First" "One" 10)
                insertEntity personTable (Person 0 "Second" "Two" 20)
                insertEntity personTable (Person 0 "Third" "Three" 30)
                findAll personTable
            liftIO $ map firstName results `shouldBe` ["First", "Second", "Third"]

    describe "updateEntity" $ do
        it "updates a row and returns via findEntity" $ do
            result <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                updateEntity personTable (Person 1 "Alice" "Jones" 31)
                findEntity personTable 1
            liftIO $ result `shouldBe` Just (Person 1 "Alice" "Jones" 31)

        it "updates a widget with explicit PK" $ do
            result <- withFreshDb widgetTable $ do
                insertEntity widgetTable (Widget 10 "OldLabel")
                updateEntity widgetTable (Widget 10 "NewLabel")
                findEntity widgetTable 10
            liftIO $ result `shouldBe` Just (Widget 10 "NewLabel")

        it "does not affect other rows" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                insertEntity personTable (Person 0 "Bob" "Jones" 25)
                updateEntity personTable (Person 1 "Alice" "Updated" 99)
                findAll personTable
            liftIO $ map firstName results `shouldBe` ["Alice", "Bob"]

    describe "deleteEntity" $ do
        it "deletes a row" $ do
            result <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                deleteEntity personTable 1
                findEntity personTable 1
            liftIO $ result `shouldBe` Nothing

        it "does not delete other rows" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                insertEntity personTable (Person 0 "Bob" "Jones" 25)
                deleteEntity personTable 1
                findAll personTable
            liftIO $ map firstName results `shouldBe` ["Bob"]

        it "deleting non-existent key does nothing" $ do
            results <- withFreshDb personTable $ do
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                deleteEntity personTable 999
                findAll personTable
            liftIO $ length results `shouldBe` 1

    describe "upsertEntity" $ do
        it "inserts a new row when no conflict exists" $ do
            results <- withFreshDb widgetTable $ do
                upsertEntity widgetTable ByPrimaryKey (Widget 1 "NewWidget")
                findAll widgetTable
            liftIO $ length results `shouldBe` 1
            liftIO $ case results of
                (r : _) -> widgetLabel r `shouldBe` "NewWidget"
                [] -> error "expected one result"

        it "upserts by a unique field via ByField" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS person_unique_email ("
                        , "id INTEGER PRIMARY KEY,"
                        , "name TEXT NOT NULL,"
                        , "email TEXT NOT NULL UNIQUE"
                        , ")"
                        ]
            results <-
                withFreshDbExtras [createTable] pueTable $ do
                    insertEntity pueTable (PersonUniqueEmail 1 "Alice" "alice@example.com")
                    upsertEntity
                        pueTable
                        (ByField pueEmailField)
                        (PersonUniqueEmail 1 "Alice Updated" "alice@example.com")
                    findAll pueTable
            liftIO $ length results `shouldBe` 1
            liftIO $ case results of
                (r : _) -> pueName r `shouldBe` "Alice Updated"
                [] -> error "expected one result"

        it "updates an existing row on conflict by primary key" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER PRIMARY KEY,"
                        , "label TEXT NOT NULL"
                        , ")"
                        ]
            results <-
                withFreshDbExtras [createTable] widgetTable $ do
                    insertEntity widgetTable (Widget 10 "OldLabel")
                    upsertEntity widgetTable ByPrimaryKey (Widget 10 "NewLabel")
                    findAll widgetTable
            liftIO $ length results `shouldBe` 1
            liftIO $ case results of
                (r : _) -> widgetLabel r `shouldBe` "NewLabel"
                [] -> error "expected one result"

    describe "upsertAndReturnEntity" $ do
        it "inserts a new row and returns the entity" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER PRIMARY KEY,"
                        , "label TEXT NOT NULL"
                        , ")"
                        ]
            result <-
                withFreshDbExtras [createTable] widgetTable $ do
                    upsertAndReturnEntity widgetTable ByPrimaryKey (Widget 42 "NewWidget")
            liftIO $ result `shouldBe` Widget 42 "NewWidget"

        it "updates an existing row and returns the updated entity" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER PRIMARY KEY,"
                        , "label TEXT NOT NULL"
                        , ")"
                        ]
            result <-
                withFreshDbExtras [createTable] widgetTable $ do
                    insertEntity widgetTable (Widget 10 "OldLabel")
                    upsertAndReturnEntity widgetTable ByPrimaryKey (Widget 10 "NewLabel")
            liftIO $ result `shouldBe` Widget 10 "NewLabel"

    describe "insertOnConflictDoNothing" $ do
        it "skips insert on conflict by primary key" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER PRIMARY KEY,"
                        , "label TEXT NOT NULL"
                        , ")"
                        ]
            results <-
                withFreshDbExtras [createTable] widgetTable $ do
                    insertEntity widgetTable (Widget 10 "Existing")
                    insertOnConflictDoNothing widgetTable ByPrimaryKey (Widget 10 "ShouldNotInsert")
                    findAll widgetTable
            liftIO $ length results `shouldBe` 1
            liftIO $ case results of
                (r : _) -> widgetLabel r `shouldBe` "Existing"
                [] -> error "expected one result"

        it "inserts when no conflict exists" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER PRIMARY KEY,"
                        , "label TEXT NOT NULL"
                        , ")"
                        ]
            results <-
                withFreshDbExtras [createTable] widgetTable $ do
                    insertOnConflictDoNothing widgetTable ByPrimaryKey (Widget 10 "New")
                    findAll widgetTable
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
            results <-
                withFreshDbExtras [createTable] pueTable $ do
                    insertEntity pueTable (PersonUniqueEmail 1 "Alice" "alice@example.com")
                    insertOnConflictDoNothing
                        pueTable
                        (ByField pueEmailField)
                        (PersonUniqueEmail 2 "Bob" "alice@example.com")
                    findAll pueTable
            liftIO $ length results `shouldBe` 1
            liftIO $ case results of
                (r : _) -> pueName r `shouldBe` "Alice"
                [] -> error "expected one result"

    describe "insertOnConflictDoNothingUntargeted" $ do
        it "skips insert on any conflict without specifying a target" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER PRIMARY KEY,"
                        , "label TEXT NOT NULL"
                        , ")"
                        ]
            results <-
                withFreshDbExtras [createTable] widgetTable $ do
                    insertEntity widgetTable (Widget 10 "Existing")
                    insertOnConflictDoNothingUntargeted widgetTable (Widget 10 "ShouldNotInsert")
                    findAll widgetTable
            liftIO $ length results `shouldBe` 1
            liftIO $ case results of
                (r : _) -> widgetLabel r `shouldBe` "Existing"
                [] -> error "expected one result"

        it "inserts when no conflict exists" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER PRIMARY KEY,"
                        , "label TEXT NOT NULL"
                        , ")"
                        ]
            results <-
                withFreshDbExtras [createTable] widgetTable $ do
                    insertOnConflictDoNothingUntargeted widgetTable (Widget 1 "New")
                    findAll widgetTable
            liftIO $ length results `shouldBe` 1

    describe "ConflictTarget resolution" $ do
        it "ByMarshaller works with multi-column unique constraint" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER,"
                        , "label TEXT NOT NULL,"
                        , "UNIQUE(widget_id, label)"
                        , ")"
                        ]
            results <-
                withFreshDbExtras [createTable] widgetTable $ do
                    insertEntity widgetTable (Widget 10 "OldLabel")
                    upsertEntity
                        widgetTable
                        (ByMarshaller widgetMarshaller)
                        (Widget 10 "OldLabel")
                    findAll widgetTable
            liftIO $ length results `shouldBe` 1

        it "ByConflictTargetExpr works with a custom expression" $ do
            let createTable =
                    T.unwords
                        [ "CREATE TABLE IF NOT EXISTS widget ("
                        , "widget_id INTEGER PRIMARY KEY,"
                        , "label TEXT NOT NULL"
                        , ")"
                        ]
            results <-
                withFreshDbExtras [createTable] widgetTable $ do
                    insertEntity widgetTable (Widget 10 "OldLabel")
                    let target = conflictTargetForColumnNames ["widget_id"]
                    upsertEntity widgetTable (ByConflictTargetExpr target) (Widget 10 "NewLabel")
                    findAll widgetTable
            liftIO $ length results `shouldBe` 1
            liftIO $ case results of
                (r : _) -> widgetLabel r `shouldBe` "NewLabel"
                [] -> error "expected one result"

        it "NoPrimaryKey error for keyless table" $ do
            let keylessDef = mkTableDefinitionWithoutKey "keyless" personMarshaller
            result <-
                try $
                    withFreshDb keylessDef $
                        upsertEntity keylessDef ByPrimaryKey (Person 0 "Alice" "Smith" 30)
            liftIO $ result `shouldBe` (Left NoPrimaryKey :: Either ConflictTargetError ())
