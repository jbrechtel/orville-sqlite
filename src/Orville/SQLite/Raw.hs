{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Orville.SQLite.Raw (
    execute,
    executeWith,
    query_,
    queryWith,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3

import Orville.SQLite.Internal (getRowData)
import Orville.SQLite.Monad (OrvilleM)

-- | Execute a raw SQL statement with no parameters.
execute :: T.Text -> OrvilleM ()
execute sql = do
    db <- ask
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt

-- | Execute a raw SQL statement with bound parameters.
executeWith :: T.Text -> [SQLite3.SQLData] -> OrvilleM ()
executeWith sql params = do
    db <- ask
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt params
        _ <- SQLite3.step stmt
        SQLite3.finalize stmt

-- | Execute a raw SQL query with no parameters and collect all rows.
query_ :: T.Text -> OrvilleM [[(String, SQLite3.SQLData)]]
query_ sql = do
    db <- ask
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        let loop acc = do
                stepResult <- SQLite3.step stmt
                case stepResult of
                    SQLite3.Done -> do
                        SQLite3.finalize stmt
                        pure (reverse acc)
                    SQLite3.Row -> do
                        rowData <- getRowData stmt []
                        loop (rowData : acc)
        loop []

-- | Execute a raw SQL query with bound parameters and collect all rows.
queryWith :: T.Text -> [SQLite3.SQLData] -> OrvilleM [[(String, SQLite3.SQLData)]]
queryWith sql params = do
    db <- ask
    liftIO $ do
        stmt <- SQLite3.prepare db sql
        SQLite3.bind stmt params
        let loop acc = do
                stepResult <- SQLite3.step stmt
                case stepResult of
                    SQLite3.Done -> do
                        SQLite3.finalize stmt
                        pure (reverse acc)
                    SQLite3.Row -> do
                        rowData <- getRowData stmt []
                        loop (rowData : acc)
        loop []
