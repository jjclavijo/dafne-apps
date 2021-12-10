{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Query
import qualified Types as T
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BS

data DafneGetEv = DafneGetEv {  event :: Integer, dist :: Double, days :: Integer }                   deriving (Data,Typeable,Show)

dafnegetev = DafneGetEv { event = def
                        , dist = def
                        , days = def}

main = do
        optiones <- cmdArgs dafnegetev
        datos <- evSeriesF (event optiones) (dist optiones) (days optiones)
        putStr . (BS.toString . encode) $ datos
