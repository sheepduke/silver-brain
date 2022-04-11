{-# LANGUAGE NamedFieldPuns #-}

module SilverBrain.Util.StoreConnection where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Database.SQLite.Simple as Sqlite

type StoreConnection = Sqlite.Connection

newtype StoreConnector = StoreConnector
  { connections :: MVar (Map String StoreConnection)
  }

newConnector :: IO StoreConnector
newConnector = do
  mvar <- MVar.newMVar Map.empty
  return
    StoreConnector
      { connections = mvar
      }

getSqliteConnection :: StoreConnector -> String -> FilePath -> IO StoreConnection
getSqliteConnection StoreConnector {connections} dbName dbFilePath = do
  MVar.modifyMVar connections $ \connMap -> do
    case Map.lookup dbName connMap of
      Just conn -> return (connMap, conn)
      Nothing -> do
        conn <- Sqlite.open dbFilePath
        return (Map.insert dbName conn connMap, conn)

closeAllConnections :: StoreConnector -> IO ()
closeAllConnections StoreConnector {connections} = do
  connMap <- MVar.readMVar connections
  Map.traverseWithKey
    (\_ conn -> Sqlite.close conn)
    connMap
  return ()
