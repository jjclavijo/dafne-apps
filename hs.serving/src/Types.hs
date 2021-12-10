{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Types (CoordinateSeries,Estacion,TenvTime,TenvTimeMJD
              , EvtTime, Coordinate(..), Este, Norte
              , Altura, Posicion, Long, TenvLine
              , TenvRow(..), PtSeries(..), EvSeries(..)
              , ChunkSeries(..), ChunkProps(..)) where

import Data.Time
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Data.Text (Text)

type Estacion = String
type TenvTime = Day
type TenvTimeMJD = Integer
type EvtTime = LocalTime

--type Coordinate = Maybe Double
data Coordinate = Coordinate (Maybe Double) deriving (Show,Eq,Generic)

type Este = Coordinate
type Norte = Coordinate
type Altura = Coordinate
type Posicion = (Este,Norte,Altura) 
type Long = Double
type TenvLine = String

type CoordinateSeries = [Posicion]

data TenvRow = TenvRow { estacion :: Estacion
                       , tiempo :: TenvTime
                       , posicion :: Posicion
                       , long0 :: Long
                       }
  deriving (Show, Eq)

data PtSeries = PtSeries { name :: Estacion,
                           inicio :: TenvTime,
                           datos :: CoordinateSeries
                         }
  deriving (Show, Generic)

data EvSeries = EvSeries { --evento :: Evento,
                           tini :: TenvTime,
                           puntos :: [Estacion],
                           instances :: [CoordinateSeries]
                         }
  deriving (Show, Generic)

data ChunkSeries = ChunkSeries { --evento :: Evento,
                           chunkIni :: TenvTime,
                           chunkPts :: [Estacion],
                           chunkCoords :: [CoordinateSeries]
                         }
  deriving (Show, Generic)

data ChunkProps = ChunkProps { chunkEstaciones :: [String],
                               chunkTiempo :: Day,
                               chunkLargo :: Integer
                             }
  deriving (Show, Generic)
                                

instance ToJSON Coordinate where
  toJSON (Coordinate r) = case r of
               Nothing -> String "NaN"
               Just a -> toJSON a
-- para Implementar FROM: 
-- https://www.reddit.com/r/haskellquestions/comments/a0lfnc/how_do_you_define_a_type_that_expects_a_double/eaiuem9/

instance FromJSON Coordinate where
  parseJSON :: Value -> Parser Coordinate
  parseJSON v = quotedStringNaN v <|> stringNaN v <|> val v
    where
      stringNaN v = do
        "NaN" <- parseJSON v :: Parser Text
        -- note that if this pattern match on "NaN" fails,
        -- or if v fails to parse as a Text, the parser will
        -- fail and the alternative branch of the <|> will be
        -- tried instead
        return (Coordinate Nothing)
      quotedStringNaN v = do
        "\"NaN\"" <- parseJSON v :: Parser Text
        -- note that if this pattern match on "NaN" fails,
        -- or if v fails to parse as a Text, the parser will
        -- fail and the alternative branch of the <|> will be
        -- tried instead
        return (Coordinate Nothing)
      val v = do
        x <- parseJSON v
        return (Coordinate x)

instance ToJSON PtSeries
instance ToJSON EvSeries
instance ToJSON ChunkProps
instance ToJSON ChunkSeries

instance FromJSON PtSeries
instance FromJSON EvSeries
instance FromJSON ChunkProps
instance FromJSON ChunkSeries

