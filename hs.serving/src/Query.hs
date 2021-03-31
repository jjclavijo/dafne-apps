module Query where

import AppEnv (defaultConn)
import qualified QuerySiDB as Qdb
import qualified QueryFiles as Qf
import Types

-- import Control.Concurrent.Async

joinEvData :: [PtSeries] -> EvSeries
joinEvData series = (\(t,p,d) -> EvSeries t p d) $ unpack series
  where unpack (x:xs) = ( inicio x, map name (x:xs), map datos (x:xs))

{-
Start of IO. each of this will have its exposing API
-}

eventProps :: Integer -> IO Qdb.Event
eventProps evt = do
  conn <- defaultConn
  ev <- Qdb.eventProps conn evt
  return ev

eventPoints :: (Real a) => Integer -> a -> IO [Qdb.EvtPtPos]
eventPoints evt dist = do
  conn <- defaultConn
  ev <- Qdb.eventProps conn evt
  pts <- Qdb.eventPointsPos conn ev dist
  return pts

-- El tiempo se ingresa en el formato de tiempo
-- de los archivos. La conversion se manejará 
-- aparte si se necesita.
stSeries :: Estacion -> TenvTime -> Integer -> IO PtSeries
stSeries estacion tiempo dias = do
  rows <- Qf.readPointFile estacion tiempo dias
--  file <- Qf.readPointFileRaw estacion
--  let rows = Qf.readPoint tiempo dias file
  return (PtSeries estacion tiempo (Qf.getCoords rows) )

evSeriesList :: (Real a) => Integer -> a -> Integer -> IO [PtSeries]
evSeriesList evento distancia dias = do
  conn <- defaultConn
  props <- Qdb.eventProps conn evento
  pts <- Qdb.eventPointsPos conn props distancia
  let Just day = Qdb.evtday props
  let serie pt = stSeries (Qdb.estacion pt) day dias
  series <- mapM serie pts
  return series

evSeries :: (Real a) => Integer -> a -> Integer -> IO EvSeries
evSeries evento distancia dias = do
  seriesList <- evSeriesList evento distancia dias
  let series = joinEvData seriesList
  return series

-- Versiones F --
-- F de Filtrado,
-- Aprovechamos que la base de datos sabe que tiempos están disponibles
-- para pedirle que nos diga cual es la primera línea que tenemos
-- que mirar, entonces salteamos en el archivo todas las líneas anteriores.

-- en formato "YYMONDD"
stSeriesF :: Estacion -> String -> TenvTime -> Integer -> IO PtSeries
stSeriesF estacion desde tiempo dias = do
  rows <- Qf.readPointFileF estacion desde tiempo dias
  return (PtSeries estacion tiempo (Qf.getCoords rows) )

evSeriesListF :: (Real a) => Integer -> a -> Integer -> IO [PtSeries]
evSeriesListF evento distancia dias = do
  conn <- defaultConn
  props <- Qdb.eventProps conn evento
  pts <- Qdb.eventPointsPos conn props distancia
  let Just day = Qdb.evtday props
-- serie :: EvtPtPos -> IO PtSeries
  let serie pt = stSeriesF (Qdb.estacion pt) (Qdb.inicio pt) day dias
  series <- mapM serie pts
  return series

evSeriesF :: (Real a) => Integer -> a -> Integer -> IO EvSeries
evSeriesF evento distancia dias = do
  seriesList <- evSeriesListF evento distancia dias
  let series = joinEvData seriesList
  return series

-- TODO: Re-sort Types, EvtPtPos is criptic.
evDataAndSeries :: (Real a) => Integer -> a -> Integer -> IO (EvSeries, [Qdb.EvtPtPos])
evDataAndSeries evento distancia dias = do
  conn <- defaultConn
  props <- Qdb.eventProps conn evento
  pts <- Qdb.eventPointsPos conn props distancia
  let Just day = Qdb.evtday props
--
-- serie :: EvtPtPos -> IO PtSeries
  let serie pt = stSeriesF (Qdb.estacion pt) (Qdb.inicio pt) day dias
  seriesList <- mapM serie pts
--
  let series = joinEvData seriesList
--
  return (series, pts)

-- TODO: We repeat Querys a Lot, 
-- Template Haskell could do it just like $(allquerys q1 q2 q3) or something?


