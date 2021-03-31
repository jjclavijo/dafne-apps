{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( startApp
    , app
    , www
    , api
    ) where


-- External imports
import Data.Aeson
import Servant
import Network.Wai.Handler.Warp (run)
import GHC.Generics
import Control.Monad.Except
import System.FilePath

-- Local Imports
import Query
import Clients
import qualified Conversion as C
import qualified Types as T
import qualified Process as P

-- $(deriveJSON defaultOptions ''User) Keep it, TemplateHaskell for adding From/To JSON instances.

-- The event handling API, predict and predictSteps depends upon
-- differente tensorflow models. TODO: a new predict based on data
-- and steps.

type API = "get_event" :> Capture "event" Integer :> Capture "dist" Double :> Capture "days" Integer :> Get '[JSON] T.EvSeries
      :<|> "predict" :> Capture "event" Integer :> Capture "dist" Double :> Get '[JSON] (Maybe EvPreds)
      :<|> "predictSteps" :> Capture "event" Integer :> Capture "dist" Double :> Get '[JSON] (Maybe EvSteps)
      :<|> "map" :> Capture "event" Integer :> Capture "dist" Double :> Get '[JSON] (Maybe P.EvtPtsGeo)

api :: Proxy API
api = Proxy

server :: Server API
server = getEvent
    :<|> predict
    :<|> predictSteps
    :<|> map
        where getEvent a b c = liftIO $ evSeriesF a b c
              predict ev dist = liftIO $ runPrediction =<< evSeriesF ev dist 30
              predictSteps ev dist = liftIO $ runPredictionSteps =<< evSeriesF ev dist 30
              map ev dist = liftIO $ P.predictAndMap ev dist 30

-- TODO: 30 hardcoded, must be somewhere in "clients"

-- Static files
-- mapOneEvent.html has a view for generated geojson
-- data, using Leaflet.js (and jquery.min.js)
--
-- TODO: A view (perhaps D3.js) for time-series related data.

type API' = API -- The API we want a JS handler for
           :<|> Raw     -- used for serving static files

-- this proxy targets everything
api' :: Proxy API'
api' = Proxy

-- * Server-side handler

-- where our static files reside
www :: FilePath
www = "www"

-- defining handlers
--server :: TVar Counter -> Server TestApi
--server counter = counterPlusOne counter     -- (+1) on the TVar
--            :<|> currentValue counter       -- read the TVar

server' :: Server API'
server'= server                     -- API Server
    :<|> serveDirectory www         -- serve static files


--main :: IO ()
--main = do
--  -- write the JS code to www/api.js at startup
--  writeJSForAPI testApi jquery (www </> "api.js")

app :: Application
app = serve api' server'

startApp :: IO ()
startApp = run 8080 app
