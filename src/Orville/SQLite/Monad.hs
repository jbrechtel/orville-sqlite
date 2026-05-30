{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Orville.SQLite.Monad (
    OrvilleM,
    withConnection,
    openConnection,
    closeConnection,
    runOrvilleM,
    withTransaction,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3

newtype OrvilleM a = OrvilleM
    { unOrvilleM :: ReaderT SQLite3.Database IO a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader SQLite3.Database)

runOrvilleM :: SQLite3.Database -> OrvilleM a -> IO a
runOrvilleM db action = runReaderT (unOrvilleM action) db

withConnection :: SQLite3.Database -> OrvilleM a -> IO a
withConnection = runOrvilleM

openConnection :: String -> IO SQLite3.Database
openConnection path = SQLite3.open (T.pack path)

closeConnection :: SQLite3.Database -> IO ()
closeConnection = SQLite3.close

withTransaction :: OrvilleM a -> OrvilleM a
withTransaction action = do
    db <- ask
    liftIO $ SQLite3.exec db "BEGIN TRANSACTION"
    result <- action
    liftIO $ SQLite3.exec db "COMMIT"
    pure result
