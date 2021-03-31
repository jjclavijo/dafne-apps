{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Process where

import GHC.Generics

import QuerySiDB as Qdb

import Query as Q 
import Clients (EvSteps(..),runPredictionSteps)

import Types (Posicion,Coordinate(..), EvSeries)

import Leaflet as Symbols

import Data.Aeson
import Data.Text (Text,unpack)
import qualified Data.ByteString.Lazy as B

-- TEST
import AppEnv

data GjGeometry = GjGeometry { coordinates :: [Double] }
  deriving (Show,Generic)

instance ToJSON GjGeometry where
  toJSON GjGeometry {..} =
    object [ "coordinates" .= toJSON coordinates
           , "type"  .= toJSON ("Point"::Text) ]


data GjProperties = GjProperties { anchor_x :: Double,
                                   anchor_y :: Double,
                                   html :: Text }
  deriving (Show,Generic)

instance ToJSON GjProperties 

data GeoJsonFeature = GeoJsonFeature { geometry :: Maybe GjGeometry,
                                       properties :: Maybe GjProperties}
  deriving (Show,Generic)

instance ToJSON GeoJsonFeature where
  toJSON GeoJsonFeature {..} =
    object [ "geometry" .= toJSON geometry
           , "properties" .= toJSON properties
           , "type"  .= toJSON ("Feature"::Text) ]

data EvtPtsGeo = EvtPtsGeo { features :: [GeoJsonFeature] }
  deriving (Show,Generic)

instance ToJSON EvtPtsGeo where
  toJSON EvtPtsGeo {..} =
    object [ "features" .= toJSON features
           , "type"  .= toJSON ("FeatureCollection"::Text) ]


toGeometry :: Qdb.EvtPtPos -> GjGeometry
toGeometry Qdb.EvtPtPos 
  {Qdb.long=lon, Qdb.lat=la} = GjGeometry { coordinates = [lon , la] }

--toStepProps :: Posicion -> GjProperties

--geoStations ::

--geoStationsSteps :: 

--TEST
--
convertFst :: [Qdb.EvtPtPos] -> GjGeometry
convertFst (x:xs) = toGeometry x

convert1 :: Qdb.EvtPtPos -> GjGeometry
convert1 x = toGeometry x

convertMany :: [Qdb.EvtPtPos] -> EvtPtsGeo
convertMany xs = EvtPtsGeo [ GeoJsonFeature {geometry=Just (convert1 x),
                                             properties=Nothing} |
                             x <- xs ]

stepToProps :: Posicion -> Maybe GjProperties
stepToProps (Coordinate (Just e),Coordinate (Just n),Coordinate (Just u)) = Just GjProperties {anchor_x = 20 * mag e n, anchor_y = 20 * mag e n,
                  html = Symbols.svgArrow (mag e n)
                                          (180 * (atan2 e n) / pi) 
                   }
  where mag a b = (1e1 * (a ** 2 + b ** 2) ** 0.5)

mergeFst :: [Qdb.EvtPtPos] -> Maybe EvSteps -> GeoJsonFeature
mergeFst (x:xs) (Just EvSteps {predictions=(y:ys)}) =
  GeoJsonFeature {geometry= Just (convert1 x),
                  properties= stepToProps y}


mergeAll :: [Qdb.EvtPtPos] -> Maybe EvSteps -> Maybe EvtPtsGeo
mergeAll (xs) (Just EvSteps {predictions=(ys)}) = Just $ EvtPtsGeo [
  GeoJsonFeature {geometry= Just (convert1 x),
                  properties= stepToProps y} | x <- xs | y <- ys ]
mergeAll a b = Nothing


--main :: IO ()
--main = do
--  conn <- defaultConn
--  ev <- Qdb.eventProps conn 2639
--  pts <- Qdb.eventPointsPos conn ev 2000000
----  B.putStr (encode . convertMany $ pts)
----
--  series <- Q.evSeriesF 2639 2000000 30
--  steps <- runPredictionSteps series 
--  B.putStr ( encode $ mergeAll pts steps)
----  B.putStr ( encode steps )

-- TODONOW:
--

predictAndMap :: (Real a) => Integer -> a -> Integer -> IO (Maybe EvtPtsGeo)
predictAndMap ev dist eps = do
  (series, pts) <- Q.evDataAndSeries ev dist eps
  steps <- runPredictionSteps series 
  return (mergeAll pts steps)
--                 mergeAll =<< <$> Q.evDataAndSeries evento distancia dias) $ runPredictionSteps =<< (fst <$> Q.evDataAndSeries evento distancia dias)

--main :: IO ()
--main = B.putStr . encode =<< predictAndMap 2639 2000000 30
