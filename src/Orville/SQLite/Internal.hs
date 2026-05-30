{-# LANGUAGE ScopedTypeVariables #-}

module Orville.SQLite.Internal (
    getRowData,
) where

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
    mapM
        ( \i -> do
            let idx :: Int = fromIntegral i
                colName =
                    if idx < length cols
                        then cols !! idx
                        else ""
            sqlVal <- SQLite3.column stmt i
            pure (colName, sqlVal)
        )
        indexes
