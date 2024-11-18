module Main (main) where

import Relude

import Control.Concurrent (myThreadId)
import Control.Exception.Safe (throwTo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setOnException, setPort)
import Network.Wai.Logger (withStdoutLogger)

import Jukebox.API (app)
import Jukebox.Rooms (emptyRooms)

main :: IO ()
main = withStdoutLogger $ \logger -> do
  -- I deliberately want to crash the main thread on exceptions to make
  -- debugging easier.
  mainThreadId <- myThreadId
  appState <- newTVarIO emptyRooms
  putStrLn "Starting server at http://localhost:8081"
  let settings =
        defaultSettings
          & setLogger logger
          & setOnException (const $ throwTo mainThreadId)
          & setPort 8081
  runSettings settings $ app appState
