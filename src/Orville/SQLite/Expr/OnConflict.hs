{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.SQLite.Expr.OnConflict
    ( OnConflictExpr
    , ConflictTargetExpr
    , conflictTargetForColumnNames
    , onConflictDoUpdate
    , onConflictDoNothing
    , onConflictDoNothingUntargeted
    ) where

import Data.List (intercalate)
import Orville.SQLite.RawSql (RawSql)
import qualified Orville.SQLite.RawSql as RawSql

-- | Represents the SQL 'ON CONFLICT <target> <action>' clause.
newtype OnConflictExpr
    = OnConflictExpr RawSql.RawSql
    deriving (Show, Eq, Semigroup, Monoid)

-- | Represents the conflict target portion: @ON CONFLICT (<target>)@.
newtype ConflictTargetExpr
    = ConflictTargetExpr RawSql.RawSql
    deriving (Show, Eq, Semigroup, Monoid)

-- | Build a 'ConflictTargetExpr' from a list of column names.
conflictTargetForColumnNames :: [String] -> ConflictTargetExpr
conflictTargetForColumnNames colNames =
    ConflictTargetExpr $
        RawSql.leftParen
            <> RawSql.fromString (intercalate ", " colNames)
            <> RawSql.rightParen

-- | Build an 'OnConflictExpr' that performs @ON CONFLICT <target> DO UPDATE SET
-- col1 = excluded.col1, col2 = excluded.col2, ...@
onConflictDoUpdate :: ConflictTargetExpr -> [String] -> OnConflictExpr
onConflictDoUpdate targetExpr colNames =
    OnConflictExpr $
        RawSql.fromString "ON CONFLICT "
            <> targetRaw targetExpr
            <> RawSql.fromString " DO UPDATE SET "
            <> RawSql.fromString (intercalate ", " setClauses)
  where
    setClauses =
        [ colName <> " = excluded." <> colName
        | colName <- colNames
        ]

-- | Build an 'OnConflictExpr' that performs @ON CONFLICT <target> DO NOTHING@.
onConflictDoNothing :: ConflictTargetExpr -> OnConflictExpr
onConflictDoNothing targetExpr =
    OnConflictExpr $
        RawSql.fromString "ON CONFLICT "
            <> targetRaw targetExpr
            <> RawSql.fromString " DO NOTHING"

-- | Extract the underlying 'RawSql' from a 'ConflictTargetExpr'.
targetRaw :: ConflictTargetExpr -> RawSql
{-# INLINE targetRaw #-}
targetRaw (ConflictTargetExpr r) = r

-- | Build an 'OnConflictExpr' that performs @ON CONFLICT DO NOTHING@ (no target).
onConflictDoNothingUntargeted :: OnConflictExpr
onConflictDoNothingUntargeted =
    OnConflictExpr $ RawSql.fromString "ON CONFLICT DO NOTHING"
