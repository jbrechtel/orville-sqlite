{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orville.SQLite.TableDefinition (
    PrimaryKey (..),
    primaryKey,
    TableDefinition (..),
    mkTableDefinition,
    mkTableDefinitionWithoutKey,
) where

import Orville.SQLite.FieldDefinition (
    FieldDefinition,
    Nullability (..),
    convertField,
    integerField,
 )
import Orville.SQLite.SqlMarshaller (SqlMarshaller)

data PrimaryKey writeEntity key where
    PrimaryKey ::
        (writeEntity -> key) ->
        FieldDefinition 'NotNull key ->
        PrimaryKey writeEntity key

primaryKey ::
    (writeEntity -> key) ->
    FieldDefinition 'NotNull key ->
    PrimaryKey writeEntity key
primaryKey = PrimaryKey

data TableDefinition key writeEntity readEntity = TableDefinition
    { tableName :: String
    , tablePrimaryKey :: PrimaryKey writeEntity key
    , tableMarshaller :: SqlMarshaller writeEntity readEntity
    }

mkTableDefinition ::
    String ->
    PrimaryKey writeEntity key ->
    SqlMarshaller writeEntity readEntity ->
    TableDefinition key writeEntity readEntity
mkTableDefinition = TableDefinition

mkTableDefinitionWithoutKey ::
    String ->
    SqlMarshaller writeEntity readEntity ->
    TableDefinition () writeEntity readEntity
mkTableDefinitionWithoutKey name marshaller =
    let
        dummyField ::
            FieldDefinition 'NotNull ()
        dummyField =
            convertField (\_ -> ()) (\() -> 0) (integerField "__rowid__")
     in
        TableDefinition
            { tableName = name
            , tablePrimaryKey = PrimaryKey (const ()) dummyField
            , tableMarshaller = marshaller
            }
