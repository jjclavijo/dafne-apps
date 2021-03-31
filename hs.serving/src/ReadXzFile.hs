{-# LANGUAGE OverloadedStrings #-}

module ReadXzFile (readXzFile) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy as T
--import qualified Data.Text as ST
import qualified Codec.Compression.Lzma as XZ
import System.Environment

import Data.Word (Word8)

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

-- Maybe unnecessary, since all expected chars ar ASCII could've unse encoded string
readXzBytes :: B.ByteString -> [T.Text]
readXzBytes p = (map TE.decodeUtf8) . (B.split (charToWord8 '\n')) . XZ.decompress $ p

-- IO op. pasa a la api usando liftIO
-- Podria ser IO [T.Text] o IO [ByteStryng] si adaptamos los parsers
readXzFile :: FilePath -> IO [String]
readXzFile f = (return . (map T.unpack) . readXzBytes) =<< B.readFile f
