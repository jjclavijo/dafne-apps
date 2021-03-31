module Gettimes (process) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T
--import qualified Data.Text as ST
import qualified Codec.Compression.Lzma as XZ
import System.Environment

-- Unhandled errors if malformed file.
parseInt :: T.Text -> Int
parseInt t = read (T.unpack t)::Int

-- Maybe unnecessary, since all expected chars ar ASCII could've unse encoded string
readGzip :: B.ByteString -> T.Text
readGzip p = TE.decodeUtf8 . XZ.decompress $ p

{- 
-- Podria extraerse las fechas char by char. podría ser mas rápido.
-- Sobre todo si trabajamos con los Bytes directamente.

plotDate :: T.Text -> T.Text
plotDate t = T.take 7 $ T.drop 5 t

plotDates :: T.Text -> T.Text
plotDates t = T.unlines [ plotDate x | x <- T.lines t ]
-}

-- Nombre mal elegido.
unpack :: T.Text -> [T.Text]
unpack t = take 4 $ T.words t

-- Esto es un lio, si hubieramos nombrado los tipos de dato, estaríamos mejor.
addtimes :: ([[(T.Text, Int)]],[[T.Text]]) -> ([[(T.Text,Int)]],[[T.Text]])
addtimes ([[]],t:ts) = addtimes ([[(x, parseInt y),(x, parseInt y)],[]],ts)
              where [_,x,_,y] = t 

addtimes (l:lx,t:ts)  
                 | snd currtuple == ((snd lastend) + 1) = addtimes ([laststart,currtuple]:lx,ts)
                 | otherwise = addtimes ( [currtuple,currtuple]:l:lx, ts)
                 where [laststart,lastend] = l   
                       currtuple = (x,parseInt y)
                                 where [_,x,_,y] = t   

addtimes (l,ts) = (l,[[]])

-- Plotea La salida en el formato que veníamos usando, y que psql sabe.

timesText :: T.Text -> T.Text
timesText t = T.unlines [ T.unwords [est, fst s, fst e] | [s,e] <- fst $ addtimes ([[]], unpackedt ) ]
            where unpackedt = map unpack $ drop 1 $ T.lines t
                  est =  head $ head unpackedt

-- IO op. pasa a la api usando liftIO
process :: FilePath -> IO String
process f = (return . T.unpack . timesText . readGzip) =<< B.readFile f

--outprocess :: FilePath -> IO ()
--outprocess f = ( putStr . T.unpack) =<< process f
