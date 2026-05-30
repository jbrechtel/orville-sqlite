{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.SQLite.RawSql
  ( RawSql (..)
  , fromString
  , toRawSql
  , intercalate
  , fromText
  , space
  , comma
  , leftParen
  , rightParen
  , equals
  ) where

import qualified Data.Text as T

newtype RawSql = RawSql {unRawSql :: String}
  deriving (Show, Eq, Semigroup, Monoid)

fromString :: String -> RawSql
fromString = RawSql

toRawSql :: RawSql -> RawSql
toRawSql = id

fromText :: T.Text -> RawSql
fromText = RawSql . T.unpack

intercalate :: RawSql -> [RawSql] -> RawSql
intercalate sep parts = RawSql . intercalateStr (unRawSql sep) $ map unRawSql parts
  where
    intercalateStr _ [] = ""
    intercalateStr _ [x] = x
    intercalateStr s (x : xs) = x <> s <> intercalateStr s xs

space :: RawSql
space = RawSql " "

comma :: RawSql
comma = RawSql ", "

leftParen :: RawSql
leftParen = RawSql "("

rightParen :: RawSql
rightParen = RawSql ")"

equals :: RawSql
equals = RawSql " = "
