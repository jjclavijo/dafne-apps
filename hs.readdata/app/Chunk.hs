{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Query
import qualified Types as T
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.ByteString.Lazy as BL
import Control.Monad (join,liftM)
import Control.Monad.IO.Class (liftIO)

chunkOrNothing :: Maybe T.ChunkProps -> IO String
chunkOrNothing (Just x) = (BS.toString . encode) <$> chunkSeriesF x
chunkOrNothing Nothing = return "No se pudo Consultar"

propsOrNothing :: Maybe T.ChunkProps -> String
propsOrNothing (Just x) = (BS.toString . encode) x
propsOrNothing Nothing = "Propiedades Mal Formateadas"

-- main = do
--         datos <- BL.getContents
--         putStr $ (propsOrNothing . decode' @T.ChunkProps) datos
--         masdatos <- (chunkOrNothing . decode' @T.ChunkProps) datos
--         putStr $ masdatos
--

inputProps :: IO (Maybe T.ChunkProps)
inputProps = decode' @T.ChunkProps <$> BL.getContents 

main = inputProps >>= \a -> 
--       (putStr $ propsOrNothing a) >> 
       join (putStr <$> chunkOrNothing a )
