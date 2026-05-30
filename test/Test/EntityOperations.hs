{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.EntityOperations where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Text (Text)
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
