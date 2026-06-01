{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orville.SQLite.SqlMarshaller (
    SqlMarshaller,
    FieldInfo (..),
    marshallField,
    marshallReadOnlyField,
    marshallMaybe,
    marshallerFieldInfo,
    marshallerDerivedColumns,
    marshallerEncodeWrite,
    marshallerDecodeRow,
    MarshallerField (..),
    ReadOnlyColumnOption (..),
    collectFromField,
    foldMarshallerFields,
) where

import qualified Database.SQLite3 as SQLite3
import Orville.SQLite.FieldDefinition (
    FieldDefinition (..),
    Nullability (..),
    fieldColumnName,
    fieldFromSqlValue,
    fieldIsNullable,
    fieldSqlTypeName,
    fieldToSqlValue,
 )

data FieldInfo = FieldInfo
    { fieldInfoName :: String
    , fieldInfoType :: String
    , fieldInfoIsNullable :: Bool
    }

data SqlMarshaller writeEntity readEntity where
    MarshallPure :: readEntity -> SqlMarshaller writeEntity readEntity
    MarshallApply ::
        SqlMarshaller writeEntity (a -> b) ->
        SqlMarshaller writeEntity a ->
        SqlMarshaller writeEntity b
    MarshallNest ::
        (writeEntity -> a) ->
        SqlMarshaller a readEntity ->
        SqlMarshaller writeEntity readEntity
    MarshallField ::
        FieldDefinition nullability a ->
        SqlMarshaller a a
    MarshallMaybe ::
        FieldDefinition 'Nullable a ->
        SqlMarshaller (Maybe a) (Maybe a)
    MarshallReadOnly ::
        SqlMarshaller a readEntity ->
        SqlMarshaller b readEntity

instance Functor (SqlMarshaller w) where
    fmap f m = MarshallPure f `MarshallApply` m

instance Applicative (SqlMarshaller w) where
    pure = MarshallPure
    (<*>) = MarshallApply

marshallField ::
    (writeEntity -> a) ->
    FieldDefinition 'NotNull a ->
    SqlMarshaller writeEntity a
marshallField accessor fieldDef =
    MarshallNest accessor (MarshallField fieldDef)

marshallReadOnlyField ::
    FieldDefinition nullability a ->
    SqlMarshaller writeEntity a
marshallReadOnlyField fieldDef =
    MarshallReadOnly (MarshallField fieldDef)

-- | Represents a primitive field entry in a 'SqlMarshaller'. Used with
-- 'foldMarshallerFields' to iterate over the fields in a marshaller.
data MarshallerField writeEntity where
    MarshallerNatural ::
        FieldDefinition nullability a ->
        Maybe (writeEntity -> a) ->
        MarshallerField writeEntity

{- | Specifies whether read-only fields should be included when using functions
such as 'collectFromField'.
-}
data ReadOnlyColumnOption
    = IncludeReadOnlyColumns
    | ExcludeReadOnlyColumns

{- | A fold function that can be used with 'foldMarshallerFields' to collect
a value calculated from a 'FieldDefinition' via the given function. The
calculated value is added to the list of values being built.

Ignores 'MarshallReadOnly' / 'MarshallPure' entries.
-}
collectFromField ::
    ReadOnlyColumnOption ->
    (forall n a. FieldDefinition n a -> result) ->
    MarshallerField writeEntity ->
    [result] ->
    [result]
collectFromField readOnlyOption fromField entry results =
    case entry of
        MarshallerNatural fieldDef (Just _) ->
            fromField fieldDef : results
        MarshallerNatural fieldDef Nothing ->
            case readOnlyOption of
                IncludeReadOnlyColumns -> fromField fieldDef : results
                ExcludeReadOnlyColumns -> results

{- | Fold over all the 'FieldDefinition's contained within a 'SqlMarshaller'.
This can be used to collect column names, encode to SQL values, etc.
-}
foldMarshallerFields ::
    forall writeEntity readEntity acc.
    SqlMarshaller writeEntity readEntity ->
    acc ->
    (forall w. MarshallerField w -> acc -> acc) ->
    acc
foldMarshallerFields marshaller acc0 f =
    go marshaller acc0
  where
    go :: SqlMarshaller w r -> acc -> acc
    go (MarshallPure _) acc = acc
    go (MarshallApply m1 m2) acc = go m1 (go m2 acc)
    go (MarshallNest _ m) acc = go m acc
    go (MarshallField fieldDef) acc =
        f (MarshallerNatural fieldDef (Just id)) acc
    go (MarshallMaybe fieldDef) acc =
        f (MarshallerNatural fieldDef (Just id)) acc
    go (MarshallReadOnly m) acc = go m acc

marshallMaybe ::
    (writeEntity -> Maybe a) ->
    FieldDefinition 'Nullable a ->
    SqlMarshaller writeEntity (Maybe a)
marshallMaybe accessor fieldDef =
    MarshallNest accessor (MarshallMaybe fieldDef)

marshallerFieldInfo ::
    SqlMarshaller writeEntity readEntity ->
    [FieldInfo]
marshallerFieldInfo marshaller =
    reverse $ go marshaller []
  where
    go :: SqlMarshaller w r -> [FieldInfo] -> [FieldInfo]
    go (MarshallPure _) acc = acc
    go (MarshallApply m1 m2) acc = go m1 (go m2 acc)
    go (MarshallNest _ m) acc = go m acc
    go (MarshallField fieldDef) acc =
        FieldInfo
            (fieldColumnName fieldDef)
            (fieldSqlTypeName fieldDef)
            (fieldIsNullable fieldDef)
            : acc
    go (MarshallMaybe fieldDef) acc =
        FieldInfo
            (fieldColumnName fieldDef)
            (fieldSqlTypeName fieldDef)
            (True :: Bool)
            : acc
    go (MarshallReadOnly m) acc = go m acc

marshallerDerivedColumns ::
    SqlMarshaller writeEntity readEntity ->
    [String]
marshallerDerivedColumns marshaller =
    reverse $ go marshaller []
  where
    go :: SqlMarshaller w r -> [String] -> [String]
    go (MarshallPure _) acc = acc
    go (MarshallApply m1 m2) acc = go m1 (go m2 acc)
    go (MarshallNest _ m) acc = go m acc
    go (MarshallField fieldDef) acc = fieldColumnName fieldDef : acc
    go (MarshallMaybe fieldDef) acc = fieldColumnName fieldDef : acc
    go (MarshallReadOnly m) acc = go m acc

marshallerEncodeWrite ::
    SqlMarshaller writeEntity readEntity ->
    writeEntity ->
    [(String, SQLite3.SQLData)]
marshallerEncodeWrite marshaller entity =
    reverse $ go marshaller entity []
  where
    go :: SqlMarshaller w r -> w -> [(String, SQLite3.SQLData)] -> [(String, SQLite3.SQLData)]
    go (MarshallPure _) _ acc = acc
    go (MarshallApply m1 m2) w acc =
        go m1 w (go m2 w acc)
    go (MarshallNest accessor m) w acc =
        go m (accessor w) acc
    go (MarshallField fieldDef) a acc =
        (fieldColumnName fieldDef, fieldToSqlValue a fieldDef) : acc
    go (MarshallMaybe fieldDef) a acc =
        case a of
            Nothing -> (fieldColumnName fieldDef, SQLite3.SQLNull) : acc
            Just val -> (fieldColumnName fieldDef, fieldToSqlValue val fieldDef) : acc
    go (MarshallReadOnly _) _ acc = acc

marshallerDecodeRow ::
    SqlMarshaller writeEntity readEntity ->
    [(String, SQLite3.SQLData)] ->
    Either String readEntity
marshallerDecodeRow marshaller rowData =
    go marshaller rowData
  where
    go :: SqlMarshaller w r -> [(String, SQLite3.SQLData)] -> Either String r
    go (MarshallPure r) _ = Right r
    go (MarshallApply m1 m2) rd = do
        f <- go m1 rd
        a <- go m2 rd
        Right (f a)
    go (MarshallNest _ m) rd = go m rd
    go (MarshallField fieldDef) rd =
        case lookup (fieldColumnName fieldDef) rd of
            Just sqlVal -> fieldFromSqlValue sqlVal fieldDef
            Nothing ->
                Left $ "Column '" <> fieldColumnName fieldDef <> "' not found in result row"
    go (MarshallMaybe fieldDef) rd =
        case lookup (fieldColumnName fieldDef) rd of
            Just SQLite3.SQLNull -> Right Nothing
            Just sqlVal -> Just <$> fieldFromSqlValue sqlVal fieldDef
            Nothing ->
                Left $ "Column '" <> fieldColumnName fieldDef <> "' not found in result row"
    go (MarshallReadOnly m) rd = go m rd
