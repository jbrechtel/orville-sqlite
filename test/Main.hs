{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import qualified Test.AutoMigration as AutoMigration
import qualified Test.EntityOperations as EntityOperations
import qualified Test.FieldDefinition as FieldDefinition
import qualified Test.Raw as Raw
import qualified Test.SqlMarshaller as SqlMarshaller

main :: IO ()
main = hspec $ do
    describe "FieldDefinition" FieldDefinition.fieldDefinitionTests
    describe "SqlMarshaller" SqlMarshaller.sqlMarshallerTests
    describe "EntityOperations" EntityOperations.entityOperationsTests
    describe "AutoMigration" AutoMigration.autoMigrationTests
    describe "Raw" Raw.rawTests
