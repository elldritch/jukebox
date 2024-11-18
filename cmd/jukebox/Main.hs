module Main (main) where

import Relude

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

import Jukebox.API (app)
import Jukebox.Rooms (emptyRooms)

main :: IO ()
main = withStdoutLogger $ \logger -> do
  appState <- newTVarIO emptyRooms
  putStrLn "Starting server at http://localhost:8081"
  let settings =
        defaultSettings
          & setLogger logger
          & setPort 8081
  runSettings settings $ app appState
