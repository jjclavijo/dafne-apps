{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
 

module Clients (runPrediction,runPredictionSteps,EvPreds(..),EvSteps(..)) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import qualified Network.HTTP.Client as NHTTP

import Network.HTTP.Client (newManager, defaultManagerSettings, 
                            managerModifyResponse,
                            managerModifyRequest) 
import Servant.API
import Servant.Client

import Data.ByteString.Search (replace)
import qualified Data.ByteString.Lazy.Search as BLS
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL

-- import qualified Servant.Client.Streaming as S

import Types (PtSeries(..),EvSeries(..),CoordinateSeries,Posicion)
import AppEnv (defaultServingURL)

import qualified Conversion as C



-- https://kseo.github.io/posts/2017-01-23-custom-connection-manager-for-http-client.html
-- Custom Manager Settings
-- set a Manager who replaces "NaN" with NaN when requesting and NaN with "NaN" when
-- reciving responces. Thus, we can Use real JSON and quety TF Serving Rest API-ish which 
-- accepts and returns quimeric JSON

customManagerSettings = defaultManagerSettings { managerModifyResponse = naToQuoteNa,
                                           managerModifyRequest = quoteNaToNa  }

naToQuoteNa :: NHTTP.Response (IO B.ByteString) -> IO (NHTTP.Response (IO B.ByteString))
naToQuoteNa = return . (fmap modifyer)
  where modifyer a = (return . toStrict . 
                     (replace ("NaN" :: B.ByteString) 
                              ("\"NaN\"" :: B.ByteString)) ) =<< a
        toStrict :: BL.ByteString -> B.ByteString
        toStrict = B.concat . BL.toChunks

quoteNaToNa :: NHTTP.Request -> IO NHTTP.Request
quoteNaToNa = return . modify
  where modify a = a { NHTTP.requestBody = NHTTP.RequestBodyBS ( (modifyer . NHTTP.requestBody) a ) }
        modifyer (NHTTP.RequestBodyBS b) = (toStrict . 
                                          (replace ("\"NaN\"" :: B.ByteString) 
                                                   ("NaN" :: B.ByteString)) ) b
        modifyer (NHTTP.RequestBodyLBS b) = (toStrict .
                                           (BLS.replace ("\"NaN\"" :: B.ByteString) 
                                                   ("NaN" :: B.ByteString)) ) b
        modifyer b = ("" :: B.ByteString)
        toStrict :: BL.ByteString -> B.ByteString
        toStrict = B.concat . BL.toChunks

-- Start Real API. TF returns only predictions, the size of provided instances.
-- Ignores every extra JSON field on EvSeries,

-- Predictions are Just series of coordinates.
-- or Positions when asking for total displacement.

-- CoordinateSeries is the same type built for the TenvFiles->JSON API.

data EvPreds = EvPreds { predictions :: [CoordinateSeries] }
  deriving (Show,Generic)

data EvSteps = EvSteps { predictions :: [Posicion] }
  deriving (Show,Generic)


instance FromJSON EvPreds
instance ToJSON EvPreds

instance FromJSON EvSteps
instance ToJSON EvSteps

-- Takes JSON, Returns JSON (Both valid, thanks haskell).
type TfAPI = "sarlanga:predict" :> ReqBody '[JSON] EvSeries :> Post '[JSON] EvPreds
        :<|> "escalones:predict" :> ReqBody '[JSON] EvSeries :> Post '[JSON] EvSteps

-- Servant Builds the convenient functions for us.
tfapi :: Proxy TfAPI
tfapi = Proxy

predictCoords :: EvSeries -> ClientM (EvPreds)
predictSteps :: EvSeries -> ClientM (EvSteps)
(predictCoords :<|> predictSteps) = client tfapi

-- Just make it IO. Query the api and return predictions.
-- TODO: Check HARDCODED URL.!!!
runPrediction :: EvSeries -> IO (Maybe EvPreds)
runPrediction input = do
  manager' <- newManager customManagerSettings
  url <- defaultServingURL
  res <- runClientM (predictCoords input) (mkClientEnv manager' url)
  case res of
    Left err -> do 
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right out -> do
      return (Just out)

-- Just make it IO. Query the api and return predictions.
-- TODO: Check HARDCODED URL.!!!
runPredictionSteps :: EvSeries -> IO (Maybe EvSteps)
runPredictionSteps input = do
  manager' <- newManager customManagerSettings
  url <- defaultServingURL
  res <- runClientM (predictSteps input) (mkClientEnv manager' url)
  case res of
    Left err -> do 
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right out -> do
      return (Just out)
