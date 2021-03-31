module Main where


import Lib
import Servant.JS
import System.FilePath

main :: IO ()
main = do
    writeJSForAPI api jquery (www </> "api.js")
    startApp
