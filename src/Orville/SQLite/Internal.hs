{-# LANGUAGE ScopedTypeVariables #-}

module Orville.SQLite.Internal (
    getRowData,
) where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3
import Database.SQLite3.Direct (columnCount)

getRowData ::
    SQLite3.Statement ->
    [String] ->
    IO [(String, SQLite3.SQLData)]
getRowData stmt cols = do
    colCount <- columnCount stmt
    let count :: Int = fromIntegral colCount
        indexes = take count [0 :: SQLite3.ColumnIndex ..]
    forM indexes $ \i -> do
        let idx :: Int = fromIntegral i
            -- Try column name from statement metadata first, then fall back to provided names, then empty
        mName <- SQLite3.columnName stmt i
        let colName = case mName of
                Just n | not (T.null n) -> T.unpack n
                _ -> if idx < length cols then cols !! idx else ""
        sqlVal <- SQLite3.column stmt i
        pure (colName, sqlVal)
