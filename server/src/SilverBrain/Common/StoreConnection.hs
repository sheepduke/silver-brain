module SilverBrain.Common.StoreConnection where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Data.Map (Map)
import Data.Map qualified as Map
import Database.SQLite.Simple qualified as Sqlite

type StoreConnection = Sqlite.Connection

newtype StoreConnector = StoreConnector
  { connections :: MVar (Map String StoreConnection)
  }

newStoreConnector :: IO StoreConnector
newStoreConnector = do
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

withTransaction :: StoreConnection -> IO a -> IO a
withTransaction connection stmt = do
  Sqlite.withTransaction connection stmt

closeAllConnections :: StoreConnector -> IO ()
closeAllConnections StoreConnector {connections} = do
  connMap <- MVar.readMVar connections
  _ <- Map.traverseWithKey
    (\_ conn -> Sqlite.close conn)
    connMap
  return ()
