module Main (main) where

import Relude

import Data.Default (def)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

import Jukebox.API (app)

main :: IO ()
main = withStdoutLogger $ \logger -> do
  appState <- newTVarIO def
  putStrLn "Starting server at http://localhost:8081"
  let settings = defaultSettings & setLogger logger & setPort 8081
  runSettings settings $ app appState
