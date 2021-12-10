{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module QuerySiDB (eventProps, eventPointsPos, chunkPointsPos, EvtPtPos(..),evtday, Event(..)) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (LocalTimestamp, Unbounded(Finite))
import Database.PostgreSQL.Simple.Range
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ

import Data.Time --(localDay, TimeOfDay, LocalTime)
import Control.Monad (liftM)

import AppEnv (defaultConn)
import Types (ChunkProps(..))

{- 
Query event propertyes.

given ogc_id Integer, get Event long, lat and time.

-}

data Event = Event { long :: Double
                   , lat :: Double
                   , time :: PGRange LocalTimestamp
                   , shocktime :: LocalTimestamp
                   }
    deriving Show

instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field <*> field

q_event :: Query
q_event = 
   [sql| SELECT st_x(geog::geometry),
                st_y(geog::geometry),trango,time
         FROM terremotos t WHERE t.ogc_fid=? |]

eventProps :: Connection -> Integer -> IO Event
eventProps conn ev = do
--  conn <- connectPostgreSQL "host='localhost' port=5432 user='postgres' password='docker' dbname='sismoident'"
  evRow:_ <- query conn q_event (Only ev)
  return evRow

{- Querying event relevant points

Given Event and distance (Integer)

distance is not inside Event, must be providede when querying
-}

-- En principio mantenemos esta estructura de datos 
-- porque es la que se usaba originalmente en los scripts.
-- TODO: estacion, dias_efectivos, tiempo, margen.
data EvtPt = EvtPt { estacion :: String
                   , inicio :: String
                   , fin :: String
                   }
    deriving Show

instance FromRow EvtPt where
  fromRow = EvtPt <$> field <*> field <*> field

-- long, lat, time, dist -> [Points and times]
-- TODO hardcoded 50 must be INT(N*days)
q_pts :: Query
q_pts =
   [sql| SELECT estacion, to_char(min(lower(tiempo)),'YYMONDD') inicio,
                to_char(max(upper(tiempo)),'YYMONDD') fin
         FROM   (SELECT   h.estacion, t.trango * h.rango as tiempo
                 FROM     http_estaciones e
                 JOIN     (VALUES ( st_setsrid(st_makepoint(?,?),4326),
                                   ?::tsrange )) AS t(geog, trango)
         	ON       st_DWithin(e.geom,t.geog,?)
                 JOIN     http_tiempos h ON e.id = h.estacion
                 WHERE    t.trango * h.rango != 'empty') a
         GROUP BY estacion
         HAVING   sum(upper(tiempo) - lower(tiempo)) > interval '50 days'
         ORDER BY estacion |] -- sum(upper(tiempo) - lower(tiempo)) DESC |]

-- TODO: Build range from time + days.
evt_parms :: (Real a) => Event -> a -> (Double, Double, PGRange LocalTimestamp, Double)
evt_parms event dist = (long (event :: Event), lat (event :: Event), time event, (realToFrac dist))

eventPoints :: (Real a) => Connection -> Event -> a -> IO [EvtPt]
eventPoints conn ev dist = do
  ests <- query conn q_pts $ evt_parms ev dist
  return ests

-- Event point with position included. TODO: keep this only
--
--
data EvtPtPos = EvtPtPos { estacion :: String
                         , inicio :: String
                         , fin :: String
                         , long :: Double
                         , lat :: Double
                         }
    deriving Show

instance FromRow EvtPtPos where
  fromRow = EvtPtPos <$> field <*> field <*> field <*> field <*> field

-- long, lat, time, dist -> [Points and times]
-- TODO hardcoded 50 must be INT(N*days)
-- q_pts_pos :: Query
-- q_pts_pos =
--    [sql| SELECT estacion, to_char(min(lower(tiempo)),'YYMONDD') inicio,
--                 to_char(max(upper(tiempo)),'YYMONDD') fin, 
--                 st_x(geom) long, st_y(geom) lat
--          FROM   (SELECT   h.estacion, t.trango * h.rango as tiempo, 
--                           e.geom::geometry as geom
--                  FROM     http_estaciones e
--                  JOIN     (VALUES ( st_setsrid(st_makepoint(?,?),4326),
--                                    ?::tsrange )) AS t(geog, trango)
--          	ON       st_DWithin(e.geom,t.geog,?)
--                  JOIN     http_tiempos h ON e.id = h.estacion
--                  WHERE    t.trango * h.rango != 'empty') a
--          GROUP BY estacion,geom
--          HAVING   sum(upper(tiempo) - lower(tiempo)) > interval '50 days'
--          ORDER BY sum(upper(tiempo) - lower(tiempo)) DESC |]


q_pts_pos :: Query
q_pts_pos =
   [sql| SELECT tablaA.estacion,tablaA.inicio,tablaA.fin, 
                st_x(tablaB.geom) long, st_y(tablaB.geom) lat
         FROM (SELECT estacion, to_char(min(lower(tiempo)),'YYMONDD') inicio,
               to_char(max(upper(tiempo)),'YYMONDD') fin
               FROM   (SELECT   h.estacion, t.trango * h.rango as tiempo 
                       FROM     http_estaciones e
                       JOIN     (VALUES ( st_setsrid(st_makepoint(?,?),4326),
                                         ?::tsrange )) AS t(geog, trango)
         	             ON       st_DWithin(e.geom,t.geog,?)
                       JOIN     http_tiempos h ON e.id = h.estacion
                       WHERE    t.trango * h.rango != 'empty') a
               GROUP BY estacion
               HAVING   sum(upper(tiempo) - lower(tiempo)) > interval '50 days'
               ORDER BY sum(upper(tiempo) - lower(tiempo)) DESC 
               ) tablaA
         JOIN (SELECT id as estacion, geom::geometry FROM http_estaciones) tablaB
         ON tablaB.estacion = tablaA.estacion
         ORDER BY tablaA.estacion |]

--
--
q_pts_props :: Query
q_pts_props =
   [sql| SELECT tablaA.estacion,tablaA.inicio,tablaA.fin, 
                st_x(tablaB.geom) long, st_y(tablaB.geom) lat
         FROM (SELECT estacion, to_char(min(lower(tiempo)),'YYMONDD') inicio,
               to_char(max(upper(tiempo)),'YYMONDD') fin
               FROM (SELECT h.estacion, tsrange(t.ini::timestamp, date_trunc('day',now())::timestamp ) * h.rango as tiempo 
                     FROM   http_estaciones e
                     JOIN   ( VALUES (?,?) ) AS t(id, ini)
         	           ON     e.id = t.id
                     JOIN   http_tiempos h ON e.id = h.estacion
                     WHERE  tsrange(t.ini::timestamp, date_trunc('day',now())::timestamp ) * h.rango != 'empty') a
               GROUP BY estacion
               ORDER BY sum(upper(tiempo) - lower(tiempo)) DESC 
               ) tablaA
         JOIN (SELECT id as estacion, geom::geometry FROM http_estaciones) tablaB
         ON tablaB.estacion = tablaA.estacion
         ORDER BY tablaA.estacion |]
            
propsToTuple :: ChunkProps -> [(String,LocalTimestamp)]
propsToTuple props = zip (chunkEstaciones props) (repeat (getTs props))
  where getTs p = Finite $ LocalTime (chunkTiempo p) (dayFractionToTimeOfDay 0)

eventPointsPos :: (Real a) => Connection -> Event -> a -> IO [EvtPtPos]
eventPointsPos conn ev dist = do
  ests <- query conn q_pts_pos $ evt_parms ev dist
  return ests

chunkPointsPos :: Connection -> ChunkProps -> IO [EvtPtPos]
chunkPointsPos conn props = do
  ests <- mapM  ((liftM head) . (query conn q_pts_props) )  $ propsToTuple props
  return ests

-- Helpers

{-
-- Extract event Day from event.
evtday :: Event -> Maybe Day
evtday evt = getday $ gettime $ time evt
  where gettime (PGRange a b) = case a of
                             Inclusive c -> Just c
                             Exclusive c -> Just c
                             otherwise   -> Nothing
        getday a = case a of
                     Nothing -> Nothing
                     Just (Finite b) -> Just $ localDay b
-}

-- Extract event Day from event.
evtday :: Event -> Maybe Day
evtday evt = getday $ shocktime evt
  where getday a = case a of
          Finite b -> Just $ localDay b
          otherwise -> Nothing

-- test
--
--main :: IO ()
--main = do
--  conn <- defaultConn
--  ev <- eventProps conn 2112
--  pts <- eventPointsPos conn ev 2000000
--  putStr (show pts)
