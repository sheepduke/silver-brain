module SilverBrain.Web where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List qualified as List
import Data.String.Conversions
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Types
import SilverBrain.Common.StoreConnection qualified as StoreConnection
import SilverBrain.ConceptMap
import SilverBrain.ConceptMap qualified as ConceptMap
import System.Directory qualified as Directory
import Web.Scotty

run :: IO ()
run = do
  storeConnector <- StoreConnection.newStoreConnector
  scotty 3000 $ do
    get "/api/concepts/:uuid" $ do
      -- TODO Read HTTP header for store name.
      uuid <- param "uuid"
      conceptProperties <- splitQueryParameters <$> rescue (param "conceptProps") (\_ -> pure "")
      conceptMap <- newConceptMap storeConnector
      conceptResult <-
        liftIO $
          ConceptMap.getConceptByUuid
            conceptMap
            uuid
            GetConceptOptions
              { conceptProperties
              }
      dataOrError conceptResult
  where
    dataOrError (Right obj) = do
      status status200
      json obj
    dataOrError (Left (UuidNotFound uuid)) = do
      status status404
      text . cs $ Text.concat ["Uuid not found: ", uuid]
    dataOrError (Left (InvalidArgument reason)) = do
      status status400
      text . cs $ Text.concat ["Bad request: ", reason]

newConceptMap :: MonadIO m => StoreConnection.StoreConnector -> m ConceptMap
newConceptMap storeConnector = do
  homeDir <- liftIO $ Directory.getHomeDirectory

  storeConnection <-
    liftIO $
      StoreConnection.getSqliteConnection
        storeConnector
        storeName
        -- TODO Change to configurable prefix.
        (homeDir ++ "/temp/silver-brain/" ++ storeName ++ ".sqlite")
  return
    ConceptMap
      { storeConnection
      }
  where
    storeName = "a"

requireParam :: Parsable a => Text -> ActionM a
requireParam key = do
  rescue (param $ cs key) $
    \msg -> do
      status status400
      text msg
      finish

splitQueryParameters :: Text -> [Text]
splitQueryParameters = List.delete Text.empty . Text.splitOn ","
