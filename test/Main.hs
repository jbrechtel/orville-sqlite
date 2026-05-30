{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Text (Text)
import Orville.SQLite
import Test.Hspec

data Person = Person
    { personId :: Int64
    , firstName :: Text
    , lastName :: Text
    , age :: Int64
    }
    deriving (Show, Eq)

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
        <*> marshallField lastName lastNameField
        <*> marshallField age ageField

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
                liftIO $ mAlice `shouldBe` Just (Person 1 "Alice" "Smith" 30)
            closeConnection db

        it "finds all rows" $ do
            db <- openConnection ":memory:"
            withConnection db $ do
                autoMigrateSchema defaultOptions [schemaTable personTable []]
                insertEntity personTable (Person 0 "Alice" "Smith" 30)
                insertEntity personTable (Person 0 "Bob" "Jones" 25)
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
                liftIO $ mAlice `shouldBe` Just (Person 1 "Alice" "Jones" 31)
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
