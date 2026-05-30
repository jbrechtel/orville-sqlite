{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.SqlMarshaller where

import qualified Data.Text as T
import Test.Hspec

import qualified Database.SQLite3 as SQLite3
import Orville.SQLite
import Test.Setup

sqlMarshallerTests :: Spec
sqlMarshallerTests = do
    describe "marshallerFieldInfo" $ do
        it "enumerates all fields including read-only" $ do
            let info = marshallerFieldInfo personMarshaller
            map fieldInfoName info `shouldBe` ["age", "last_name", "first_name", "id"]

        it "reports correct types" $ do
            let info = marshallerFieldInfo personMarshaller
            let types = [(fieldInfoName i, fieldInfoType i) | i <- info]
            lookup "id" types `shouldBe` Just "INTEGER"
            lookup "first_name" types `shouldBe` Just "TEXT"
            lookup "last_name" types `shouldBe` Just "TEXT"
            lookup "age" types `shouldBe` Just "INTEGER"

    describe "marshallerDerivedColumns" $ do
        it "lists all column names" $ do
            let cols = marshallerDerivedColumns personMarshaller
            length cols `shouldBe` 4

    describe "marshallerEncodeWrite" $ do
        it "excludes read-only fields" $ do
            let p = Person 0 "Alice" "Smith" 30
            let pairs = marshallerEncodeWrite personMarshaller p
            let names = map fst pairs
            names `shouldNotContain` ["id"]

        it "includes all write fields" $ do
            let p = Person 0 "Alice" "Smith" 30
            let pairs = marshallerEncodeWrite personMarshaller p
            let names = map fst pairs
            names `shouldBe` ["age", "last_name", "first_name"]

        it "encodes correct values" $ do
            let p = Person 0 "Alice" "Smith" 30
            let pairs = marshallerEncodeWrite personMarshaller p
            lookup "first_name" pairs `shouldBe` Just (SQLite3.SQLText "Alice")
            lookup "last_name" pairs `shouldBe` Just (SQLite3.SQLText "Smith")
            lookup "age" pairs `shouldBe` Just (SQLite3.SQLInteger 30)

        it "encodes nullable field as SQLNull when Nothing" $ do
            let t = Task 0 "No due" Nothing
            let pairs = marshallerEncodeWrite taskMarshaller t
            lookup "due_day" pairs `shouldBe` Just SQLite3.SQLNull

        it "encodes nullable field as SQLInteger when Just" $ do
            let t = Task 0 "Has due" (Just 10)
            let pairs = marshallerEncodeWrite taskMarshaller t
            lookup "due_day" pairs `shouldBe` Just (SQLite3.SQLInteger 10)

    describe "marshallerDecodeRow" $ do
        it "decodes a complete row" $ do
            let row =
                    [ ("id", SQLite3.SQLInteger 1)
                    , ("first_name", SQLite3.SQLText "Alice")
                    , ("last_name", SQLite3.SQLText "Smith")
                    , ("age", SQLite3.SQLInteger 30)
                    ]
            marshallerDecodeRow personMarshaller row `shouldBe` Right (Person 1 "Alice" "Smith" 30)

        it "reports missing column" $ do
            let row =
                    [ ("first_name", SQLite3.SQLText "Alice")
                    , ("last_name", SQLite3.SQLText "Smith")
                    , ("age", SQLite3.SQLInteger 30)
                    ]
            marshallerDecodeRow personMarshaller row `shouldSatisfy` \case
                Left e -> "id" `T.isInfixOf` T.pack e
                Right _ -> False

    describe "marshallReadOnlyField" $ do
        it "is included in decode but excluded from encode" $ do
            let p = Person 0 "Alice" "Smith" 30
            let pairs = marshallerEncodeWrite personMarshaller p
            let colNames = marshallerDerivedColumns personMarshaller
            colNames `shouldContain` ["id"] -- id appears in SELECT
            map fst pairs `shouldNotContain` ["id"] -- id not in INSERT values
    describe "marshallMaybe" $ do
        it "decodes SQLNull as Nothing" $ do
            let row =
                    [ ("task_id", SQLite3.SQLInteger 1)
                    , ("description", SQLite3.SQLText "Test")
                    , ("due_day", SQLite3.SQLNull)
                    ]
            marshallerDecodeRow taskMarshaller row
                `shouldBe` Right (Task 1 "Test" Nothing)

        it "decodes SQLInteger as Just" $ do
            let row =
                    [ ("task_id", SQLite3.SQLInteger 1)
                    , ("description", SQLite3.SQLText "Test")
                    , ("due_day", SQLite3.SQLInteger 25)
                    ]
            marshallerDecodeRow taskMarshaller row
                `shouldBe` Right (Task 1 "Test" (Just 25))
