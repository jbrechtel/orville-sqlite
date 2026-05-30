{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.FieldDefinition where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import Test.Hspec

import qualified Database.SQLite3 as SQLite3
import Orville.SQLite

fieldDefinitionTests :: Spec
fieldDefinitionTests = do
    describe "integerField" $ do
        it "roundtrips encode/decode" $ do
            let fd = integerField "count"
            let val = 42 :: Int64
            fieldFromSqlValue (fieldToSqlValue val fd) fd `shouldBe` Right val

        it "encodes to SQLInteger" $ do
            let fd = integerField "count"
            fieldToSqlValue 99 fd `shouldBe` SQLite3.SQLInteger 99

        it "decodes from SQLInteger" $ do
            let fd = integerField "count"
            fieldFromSqlValue (SQLite3.SQLInteger 99) fd `shouldBe` Right 99

    describe "textField" $ do
        it "roundtrips encode/decode" $ do
            let fd = textField "name"
            let val = "Hello" :: T.Text
            fieldFromSqlValue (fieldToSqlValue val fd) fd `shouldBe` Right val

        it "encodes to SQLText" $ do
            let fd = textField "name"
            fieldToSqlValue "test" fd `shouldBe` SQLite3.SQLText "test"

        it "decodes from SQLText" $ do
            let fd = textField "name"
            fieldFromSqlValue (SQLite3.SQLText "decoded") fd `shouldBe` Right ("decoded" :: T.Text)

    describe "realField" $ do
        it "roundtrips encode/decode" $ do
            let fd = realField "price"
            let val = 3.14 :: Double
            fieldFromSqlValue (fieldToSqlValue val fd) fd `shouldBe` Right val

        it "encodes to SQLFloat" $ do
            let fd = realField "price"
            fieldToSqlValue 2.718 fd `shouldBe` SQLite3.SQLFloat 2.718

    describe "blobField" $ do
        it "roundtrips encode/decode" $ do
            let fd = blobField "data"
            let val = "binary\0stuff" :: BS.ByteString
            fieldFromSqlValue (fieldToSqlValue val fd) fd `shouldBe` Right val

    describe "nullableField" $ do
        it "is marked as nullable" $ do
            let fd = nullableField (integerField "opt")
            fieldIsNullable fd `shouldBe` True

        it "retains the column name" $ do
            let fd = nullableField (textField "notes")
            fieldColumnName fd `shouldBe` "notes"

    describe "convertField" $ do
        it "converts between types" $ do
            let fd = integerField "count"
            let converted = convertField show (read @Int64) fd :: FieldDefinition 'NotNull String
            fieldFromSqlValue (SQLite3.SQLInteger 123) converted `shouldBe` Right "123"

    describe "fieldColumnName" $ do
        it "returns the column name for non-null field" $ do
            fieldColumnName (integerField "my_col") `shouldBe` "my_col"

        it "returns the column name for nullable field" $ do
            fieldColumnName (nullableField (textField "opt_col")) `shouldBe` "opt_col"

    describe "fieldSqlTypeName" $ do
        it "returns INTEGER for integerField" $ do
            fieldSqlTypeName (integerField "x") `shouldBe` "INTEGER"

        it "returns TEXT for textField" $ do
            fieldSqlTypeName (textField "x") `shouldBe` "TEXT"

        it "returns REAL for realField" $ do
            fieldSqlTypeName (realField "x") `shouldBe` "REAL"

        it "returns BLOB for blobField" $ do
            fieldSqlTypeName (blobField "x") `shouldBe` "BLOB"
