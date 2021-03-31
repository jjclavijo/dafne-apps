module QueryFiles where

import Database.PostgreSQL.Simple.Time (LocalTimestamp)
import TenvParser 
import ReadXzFile (readXzFile)

import Types

import Data.Time (addDays,diffDays,fromGregorian)


-- parseTenvLine :: TenvLine -> TenvRow

readPointFileRaw :: Estacion -> IO [TenvLine]
-- Read Tenv file line by line, posibbly check something
readPointFileRaw estacion = do
  let file = "./data/" ++ estacion ++ ".IGS14.tenv3.xz"
  l <- readXzFile file
--  text <- readXzFile file
--  let l = lines text
  return (l)


readAllPoint :: [Maybe TenvLine] -> [Maybe TenvRow]
readAllPoint tenv = [ parse x | x <- tenv ]
  where parse a = case a of
            Nothing -> Nothing
            Just a -> parseTenvLine a

getCoords :: [Maybe TenvRow] -> CoordinateSeries
-- Convert to Coordinate Series, propagates Nothing's
getCoords trows = [ getSingleCoord x | x <- trows ]
  where getSingleCoord :: Maybe TenvRow -> Posicion
        getSingleCoord row = case row of
                   Just a -> posicion a
                   Nothing -> (Coordinate Nothing,Coordinate  Nothing,Coordinate  Nothing)


splitByDate :: ([TenvLine],Maybe TenvLine,[TenvLine]) 
               -> TenvTime 
               -> ([TenvLine],Maybe TenvLine,[TenvLine])
{- SplitByDate, given [TenvLine] and date, split tenv file at
given date, which may not be available -}
splitByDate (x,Nothing,y:ys) time = 
  case parseTenvDate y of
    Nothing -> splitByDate (x,Nothing,ys) time
    Just a  
      | a > time  -> (x,Nothing,y:ys)
      | a == time -> (x,Just y,ys)
      -- Do not return, perhaps there are many xs to go.
      | a < time  -> splitByDate (x ++ [y],Nothing,ys) time
--
splitByDate (x,Just y,z:zs) time = 
  case parseTenvDate z of
    Nothing -> splitByDate (x,Just y,zs) time
    Just a   
      | a > time  -> (x++[y],Nothing,z:zs)
      | a == time -> (x++[y],Just z,zs)
      -- Do not return, perhaps there are many xs to go.
      | a < time -> splitByDate (x ++ [y] ++ [z],Nothing,zs) time
--
splitByDate (x,y,[]) time = 
  case y of
    Nothing -> (x,y,[])
    Just a  -> (x ++ [a],Nothing,[])

timeMJD :: TenvTime -> TenvTimeMJD
timeMJD time = diffDays time (fromGregorian 1858 11 17)

dateAndRest :: (Maybe TenvLine,[TenvLine]) 
               -> TenvTime 
               -> (Maybe TenvLine,[TenvLine])
{- Search a list of TenvLine until desired date. -}
dateAndRest (Nothing,y:ys) time = 
  case parseTenvMJD y of
    Nothing -> dateAndRest (Nothing,ys) time
    Just a  
      | a > (timeMJD time) -> (Nothing,y:ys)
      | a == (timeMJD time) -> (Just y,ys)
      -- Do not return, perhaps there are many xs to go.
      | a < (timeMJD time)  -> dateAndRest (Nothing,ys) time
--
dateAndRest (Just y,z:zs) time = 
  case parseTenvMJD z of
    Nothing -> dateAndRest (Just y,zs) time
    Just a   
      | a > (timeMJD time)  -> (Nothing,z:zs)
      | a == (timeMJD time) -> (Just z,zs)
      -- Do not return, perhaps there are many xs to go.
      | a < (timeMJD time) -> dateAndRest (Nothing,zs) time
--
dateAndRest (y,[]) time = 
  case y of
    Nothing -> (y,[])
    Just a  -> (Nothing,[])


readPoint :: TenvTime -> Integer -> [TenvLine] -> [Maybe TenvRow]
readPoint time days lines = readAllPoint $ map (fst . fst) $ gettimes ( (Nothing,lines), [ addDays x time | x <- [-days..days] ] )
  where gettimes (tuple,d:days) =  (dateAndRest tuple d,days) : (gettimes (dateAndRest tuple d,days))  -- Parse Lines, crop only ndays Around Evt. Fixed to 30-1-30 by now.
        gettimes (tuple,[]) = []

readPointFile :: Estacion -> TenvTime -> Integer -> IO [Maybe TenvRow]
-- Read Tenv file line by line, posibbly check something
readPointFile estacion time days = do
  let file = "./data/" ++ estacion ++ ".IGS14.tenv3.xz"
  text <- readXzFile file
  let l = (readPoint time days) text
  return (l)

readPointFileF :: Estacion -> String -> TenvTime -> Integer -> IO [Maybe TenvRow]
-- Read Tenv file line by line, posibbly check something
readPointFileF estacion inicio time days = do
    let file = "./data/" ++ estacion ++ ".IGS14.tenv3.xz"
    text <- readXzFile file
    let l = ((readPoint time days) . (filter inicio)) text
    return (l)
  where filter :: String -> [String] -> [String]
        filter st ls = (take 70) . (dropWhile (dateis st)) $ ls
        dateis :: String -> String -> Bool
        dateis st t = ( ((take 7) . (drop 5)) t) /= st
