{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SilverBrain.Web where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types
import SilverBrain.ConceptMap (ErrorType (UuidNotFound))
import qualified SilverBrain.ConceptMap as ConceptMap
import qualified SilverBrain.ConceptMap.Core.Concept as Concept
import SilverBrain.Util.RequestContext (RequestContext (RequestContext))
import qualified SilverBrain.Util.RequestContext as RequestContext
import SilverBrain.Util.ServerContext (ServerContext)
import qualified SilverBrain.Util.ServerContext as ServerContext
import SilverBrain.Util.StoreConnection
import qualified SilverBrain.Util.StoreConnection as StoreConnection
import qualified System.Directory
import Web.Scotty

run :: IO ()
run = do
  storeConnector <- StoreConnection.newConnector
  let serverContext = ServerContext.new storeConnector
      conceptMap = ConceptMap.new serverContext
   in scotty 3000 $ do
        get "/api/concepts/:uuid" $ do
          -- TODO Read HTTP header for store name.
          conn <- liftIO $ getStoreConnection storeConnector "a"
          uuid <- param "uuid"
          let requestContext = RequestContext.new conn
          concept <- liftIO $ ConceptMap.getConceptByUuid conceptMap requestContext uuid
          dataOrError concept
  where
    getStoreConnection storeConnector storeName = do
      homeDir <- System.Directory.getHomeDirectory
      StoreConnection.getSqliteConnection
        storeConnector
        storeName
        -- TODO Change to configurable prefix.
        (homeDir ++ "/temp/silver-brain/" ++ storeName ++ ".sqlite")

    -- TODO Use real JSON here.
    dataOrError (Right obj) = do
      status status200
      text . cs . Concept.name $ obj
    dataOrError (Left UuidNotFound) = do
      status status404
