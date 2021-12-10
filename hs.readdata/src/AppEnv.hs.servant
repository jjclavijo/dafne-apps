{-# LANGUAGE OverloadedStrings #-}

module AppEnv where

import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)
import Data.ByteString.Char8 (pack)

import Text.Read (readMaybe)

import Servant.Client (BaseUrl(..), Scheme(..))


defaultServingHost :: Maybe String -> String
defaultServingHost host = case host of
                      Just a -> a
                      Nothing -> "localhost"

defaultServingPort :: Maybe String -> Int
defaultServingPort = readPort . validatePort
  where validatePort :: Maybe String -> Maybe Int
        validatePort a = case a of
                          Nothing -> Just 8501
                          Just b -> readMaybe b
        readPort :: Maybe Int -> Int
        readPort port = case port of
                          Just a -> a
                          Nothing -> 8501

defaultServingPath :: Maybe String -> String
defaultServingPath path = case path of
                      Just a -> a
                      Nothing -> "v1/models"

defaultServingURL :: IO BaseUrl
defaultServingURL = do
                      host <- defaultServingHost <$> lookupEnv "SERVING_HOST"
                      port <- defaultServingPort <$> lookupEnv "SERVING_PORT"
                      path <- defaultServingPath <$> lookupEnv "SERVING_PATH"
                      return (BaseUrl Http host port path)


defaultConnString :: Maybe String -> String
defaultConnString dafnedb = case dafnedb of
                      Just a -> a
                      Nothing -> "host='localhost' port=5432 user='postgres' password='docker' dbname='sismoident'"

defaultConn :: IO Connection
defaultConn = connectPostgreSQL =<< ((pack . defaultConnString) <$> lookupEnv "DAFNE_DB")
