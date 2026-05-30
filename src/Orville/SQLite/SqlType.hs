{-# LANGUAGE LambdaCase #-}

module Orville.SQLite.SqlType
  ( SqlType (..)
  , integerType
  , textType
  , realType
  , blobType
  , convertSqlType
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3

data SqlType a = SqlType
  { sqlTypeName :: String
  , sqlTypeToSql :: a -> SQLite3.SQLData
  , sqlTypeFromSql :: SQLite3.SQLData -> Either String a
  }

integerType :: SqlType Int64
integerType =
  SqlType
    { sqlTypeName = "INTEGER"
    , sqlTypeToSql = SQLite3.SQLInteger
    , sqlTypeFromSql = \case
        SQLite3.SQLInteger i -> Right i
        SQLite3.SQLNull -> Right 0
        other -> Left $ "Expected INTEGER, got " <> show (sqlDataKind other)
    }

textType :: SqlType T.Text
textType =
  SqlType
    { sqlTypeName = "TEXT"
    , sqlTypeToSql = SQLite3.SQLText
    , sqlTypeFromSql = \case
        SQLite3.SQLText t -> Right t
        SQLite3.SQLNull -> Right T.empty
        SQLite3.SQLInteger i -> Right (T.pack (show i))
        SQLite3.SQLFloat d -> Right (T.pack (show d))
        other -> Left $ "Expected TEXT, got " <> show (sqlDataKind other)
    }

realType :: SqlType Double
realType =
  SqlType
    { sqlTypeName = "REAL"
    , sqlTypeToSql = SQLite3.SQLFloat
    , sqlTypeFromSql = \case
        SQLite3.SQLFloat d -> Right d
        SQLite3.SQLInteger i -> Right (fromIntegral i)
        SQLite3.SQLNull -> Right 0.0
        other -> Left $ "Expected REAL, got " <> show (sqlDataKind other)
    }

blobType :: SqlType BS.ByteString
blobType =
  SqlType
    { sqlTypeName = "BLOB"
    , sqlTypeToSql = SQLite3.SQLBlob
    , sqlTypeFromSql = \case
        SQLite3.SQLBlob b -> Right b
        SQLite3.SQLNull -> Right BS.empty
        other -> Left $ "Expected BLOB, got " <> show (sqlDataKind other)
    }

convertSqlType :: (a -> b) -> (b -> a) -> SqlType a -> SqlType b
convertSqlType to from sqlType =
  SqlType
    { sqlTypeName = sqlTypeName sqlType
    , sqlTypeToSql = sqlTypeToSql sqlType . from
    , sqlTypeFromSql = fmap to . sqlTypeFromSql sqlType
    }

sqlDataKind :: SQLite3.SQLData -> String
sqlDataKind = \case
  SQLite3.SQLInteger _ -> "SQLInteger"
  SQLite3.SQLFloat _ -> "SQLFloat"
  SQLite3.SQLText _ -> "SQLText"
  SQLite3.SQLBlob _ -> "SQLBlob"
  SQLite3.SQLNull -> "SQLNull"
