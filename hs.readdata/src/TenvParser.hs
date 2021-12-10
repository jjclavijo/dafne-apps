module TenvParser (parseTenvLine,parseTenvDate,parseTenvMJD,pTime) where 

-- Ref: https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html

import Text.ParserCombinators.ReadP
import Control.Applicative
import Control.Monad (sequence)
import Text.Read (readMaybe)
import Data.Time
import Data.Time.Format
--import Database.PostgreSQL.Simple.Time (LocalTimestamp)
import Types


notspace :: ReadP Char
notspace =
    satisfy (\char -> (char /= ' ') )

alphnum :: ReadP Char
alphnum =
    satisfy (\char -> (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9') )

digit :: ReadP Char
digit =
    satisfy (\char -> (char >= '0' && char <= '9') )

digitpm :: ReadP Char
digitpm =
    digit <|> ( satisfy (\char -> ( char == '-' || char == '.' )))

site :: ReadP Estacion
site = do
    est <- count 4 alphnum
    return est

-- date Parser 2-digit year 3-char month 2-digit day
timeFormat = "%y%b%d"

understandTime = parseTimeOrError True defaultTimeLocale timeFormat

pTime :: String -> TenvTime -- LocalTime
pTime time = understandTime time

pTimeMJD :: String -> Integer
pTimeMJD time = diffDays (understandTime time) (fromGregorian 1858 11 17)

obstime :: ReadP TenvTime
obstime = do
    date <- count 7 alphnum
    return (pTime date)

-- Maybe, we should add a safeRead just if lead or tail are not
-- proper numbers. We only checked they are digits+signs+points, no order.
-- Coordinate being Maybes is for null handle later.

coordFromStrs :: String -> String -> Coordinate
coordFromStrs lead trail = Coordinate $ sumMaybe [ (readMaybe lead) , (readMaybe trail) ]
  where sumMaybe :: Num a => [Maybe a] -> Maybe a
        sumMaybe = fmap sum . sequence

coord :: ReadP Coordinate
coord = do
      lead <- many1 notspace
      skipMany1 $ string " "
      trail <- many1 notspace
      return ( coordFromStrs lead trail )

pos :: ReadP Posicion
pos = do
    east <- coord
    skipMany1 $ string " "
    north <- coord
    skipMany1 $ string " "
    up <- coord
    return ( east, north, up )

tenvrecord :: ReadP TenvRow
tenvrecord = do
    est <- site
    skipMany1 $ string " "
    t <- obstime
    count 4 (do 
      skipMany1 $ string " "
      skipMany1 notspace
      )
    skipMany1 $ string " "
    reflon <- many1 digitpm
    skipMany1 $ string " "
    pt <- pos
    string " "
    return (TenvRow est t pt ((read reflon) :: Long))

parseTenvLine :: TenvLine -> Maybe TenvRow
parseTenvLine row = case readP_to_S tenvrecord row of
            [] -> Nothing
            otherwise -> Just a
              where (a,_):_ = readP_to_S tenvrecord row

tenvtime :: ReadP TenvTime
tenvtime = do
    site
    many1 $ string " "
    t <- obstime
    string " "
    return (t)

parseTenvDate :: TenvLine -> Maybe TenvTime
parseTenvDate row = case readP_to_S tenvtime row of
            [] -> Nothing
            (a,_):_ -> Just a


parseTenvMJD :: TenvLine -> Maybe TenvTimeMJD
parseTenvMJD = (readMaybe . (take 5) . (drop 23))

parseTenvMJDold :: TenvLine -> Maybe TenvTimeMJD
parseTenvMJDold row = case readP_to_S tenvtimemjd row of
            [] -> Nothing
            (Just a,_):_ -> Just a
            (Nothing,_):_ -> Nothing
  where tenvtimemjd :: ReadP (Maybe TenvTimeMJD)
        tenvtimemjd = do
          count 23 get
          mjd <- count 5 get
          return (readMaybe mjd)

-- TODO: Drop all parsers, instead use fixed width take drop, etc.
--       which are WAY FASTER!!!!!
