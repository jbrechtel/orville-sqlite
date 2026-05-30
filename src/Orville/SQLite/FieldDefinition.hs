{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Orville.SQLite.FieldDefinition (
    Nullability (..),
    FieldDefinition (..),
    integerField,
    textField,
    realField,
    blobField,
    nullableField,
    convertField,
    fieldToSqlValue,
    fieldFromSqlValue,
    fieldColumnName,
    fieldSqlTypeName,
    fieldIsNullable,
) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Kind (Type)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3
import Orville.SQLite.SqlType (
    SqlType,
    blobType,
    convertSqlType,
    integerType,
    realType,
    sqlTypeFromSql,
    sqlTypeName,
    sqlTypeToSql,
    textType,
 )

data Nullability = NotNull | Nullable

data FieldDefinition (nullability :: Nullability) :: Type -> Type where
    NotNullField ::
        { notNullFieldName :: String
        , notNullFieldSqlType :: SqlType a
        } ->
        FieldDefinition 'NotNull a
    NullableField ::
        { nullableFieldName :: String
        , nullableFieldSqlType :: SqlType a
        } ->
        FieldDefinition 'Nullable a

fieldColumnName :: FieldDefinition null a -> String
fieldColumnName = \case
    NotNullField n _ -> n
    NullableField n _ -> n

fieldSqlTypeName :: FieldDefinition null a -> String
fieldSqlTypeName = \case
    NotNullField _ st -> sqlTypeName st
    NullableField _ st -> sqlTypeName st

fieldIsNullable :: FieldDefinition null a -> Bool
fieldIsNullable = \case
    NotNullField _ _ -> False
    NullableField _ _ -> True

fieldToSqlValue :: a -> FieldDefinition null a -> SQLite3.SQLData
fieldToSqlValue val = \case
    NotNullField _ st -> sqlTypeToSql st val
    NullableField _ st -> sqlTypeToSql st val

fieldFromSqlValue :: SQLite3.SQLData -> FieldDefinition null a -> Either String a
fieldFromSqlValue sqlVal = \case
    NotNullField _ st -> sqlTypeFromSql st sqlVal
    NullableField _ st -> sqlTypeFromSql st sqlVal

integerField :: String -> FieldDefinition 'NotNull Int64
integerField name = NotNullField name integerType

textField :: String -> FieldDefinition 'NotNull T.Text
textField name = NotNullField name textType

realField :: String -> FieldDefinition 'NotNull Double
realField name = NotNullField name realType

blobField :: String -> FieldDefinition 'NotNull BS.ByteString
blobField name = NotNullField name blobType

nullableField :: FieldDefinition 'NotNull a -> FieldDefinition 'Nullable a
nullableField = \case
    NotNullField n st -> NullableField n st

convertField ::
    (a -> b) ->
    (b -> a) ->
    FieldDefinition null a ->
    FieldDefinition null b
convertField to from = \case
    NotNullField n st -> NotNullField n (convertSqlType to from st)
    NullableField n st -> NullableField n (convertSqlType to from st)
