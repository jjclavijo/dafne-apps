{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Data.Aeson
import Data.ByteString.Lazy.Search (replace)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.ByteString (ByteString)

type TfJSON = String

replaceNaN a = replace ("\"NaN\"" :: ByteString) ("NaN" :: ByteString) a
replaceNaNq a = replace ("NaN" :: ByteString) ("\"NaN\"" :: ByteString) a

encodeForTf :: (ToJSON a) => a -> TfJSON
encodeForTf datos = (BS.toString . replaceNaN . encode) datos

decodeFromTf :: (FromJSON a) => TfJSON -> Maybe a
decodeFromTf datos = (decode . replaceNaNq . BS.fromString) datos

encodeString :: (ToJSON a) =>  a -> String
encodeString datos = (BS.toString . encode) datos

{-
encodeForTf :: (ToJSON a) => IO a -> IO String
encodeForTf datos = do
  let series = (BS.toString . replaceNaN . encode) datos
  return series

encodeString :: (ToJSON a) =>  a -> IO String
encodeString datos = do
  let series = (BS.toString . encode) datos
  return series
-}
