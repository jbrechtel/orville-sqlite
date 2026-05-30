{-# LANGUAGE OverloadedStrings #-}

module Test.Raw where

import qualified Database.SQLite3 as SQLite3
import Test.Hspec

import Orville.SQLite

rawTests :: Spec
rawTests = do
    describe "execute" $ do
        it "runs a CREATE TABLE and INSERT statement" $ do
            db <- openConnection ":memory:"
            runOrvilleM db $ execute "CREATE TABLE raw_test (id INTEGER, name TEXT)"
            runOrvilleM db $ execute "INSERT INTO raw_test VALUES (1, 'hello')"
            rows <- runOrvilleM db $ query_ "SELECT * FROM raw_test"
            length rows `shouldBe` 1
            closeConnection db

    describe "executeWith" $ do
        it "runs parameterized INSERT" $ do
            db <- openConnection ":memory:"
            runOrvilleM db $ execute "CREATE TABLE raw_test2 (id INTEGER, name TEXT)"
            runOrvilleM db $ executeWith "INSERT INTO raw_test2 VALUES (?, ?)" [SQLite3.SQLInteger 42, SQLite3.SQLText "world"]
            rows <- runOrvilleM db $ query_ "SELECT * FROM raw_test2"
            length rows `shouldBe` 1
            closeConnection db

    describe "query_" $ do
        it "returns rows from a SELECT" $ do
            db <- openConnection ":memory:"
            runOrvilleM db $ execute "CREATE TABLE raw_test3 (a INTEGER)"
            runOrvilleM db $ execute "INSERT INTO raw_test3 VALUES (1)"
            runOrvilleM db $ execute "INSERT INTO raw_test3 VALUES (2)"
            rows <- runOrvilleM db $ query_ "SELECT * FROM raw_test3 ORDER BY a"
            length rows `shouldBe` 2
            closeConnection db

    describe "queryWith" $ do
        it "returns rows matching parameters" $ do
            db <- openConnection ":memory:"
            runOrvilleM db $ execute "CREATE TABLE raw_test4 (name TEXT)"
            runOrvilleM db $ executeWith "INSERT INTO raw_test4 VALUES (?)" [SQLite3.SQLText "alpha"]
            runOrvilleM db $ executeWith "INSERT INTO raw_test4 VALUES (?)" [SQLite3.SQLText "beta"]
            rows <- runOrvilleM db $ queryWith "SELECT * FROM raw_test4 WHERE name = ?" [SQLite3.SQLText "beta"]
            length rows `shouldBe` 1
            closeConnection db
