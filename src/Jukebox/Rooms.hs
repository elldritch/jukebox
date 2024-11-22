{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Jukebox.Rooms (
  RoomID,
  ClientID,
  Rooms,
  emptyRooms,
  lookupRoom,
  Room,
  startRoom,
  startClient,
) where

import Relude
import Relude.Extra.Map (delete, elems, insert, keys, lookup, size, toPairs)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, link, link2, wait)
import Control.Concurrent.STM (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Exception.Safe (catch, catchAny, throwIO)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, eitherDecode, encode)
import Data.Map.Strict (adjust)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Show qualified as Show
import Network.WebSockets (Connection, ConnectionException (..), defaultPingPongOptions, receiveDataMessage, sendDataMessage, withPingPong)
import Network.WebSockets qualified as WS
import Servant (FromHttpApiData)
import System.IO.Unsafe (unsafePerformIO)
import Web.Sqids (SqidsOptions (..), defaultSqidsOptions, runSqids)
import Web.Sqids qualified as Sqids

-- | A `RoomID` identifies a single room. The constructor for this datatype is
-- unexported because all `RoomID`s are guaranteed to be valid when constructed
-- by functions in this module.
--
-- Since rooms can be deleted over time, so a `RoomID` is only valid for some
-- duration. They are not guaranteed to remain valid if stored and retrieved
-- later.
newtype RoomID = RoomID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromHttpApiData, ToText, ToJSON)

-- | A `ClientID` identifies a single client, which is a single WebSocket
-- connection. See `RoomID` for validity guarantees.
newtype ClientID = ClientID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromHttpApiData, ToText, ToJSON, ToJSONKey)

-- | A collection of room state.
newtype Rooms = Rooms (Map RoomID Room)

-- | An empty starting state for rooms.
emptyRooms :: Rooms
emptyRooms = Rooms mempty

-- | Look up a room given its ID. On success, returns a `RoomID` that is
-- evidence that the room exists.
lookupRoom :: Text -> Rooms -> Maybe (RoomID, Room)
lookupRoom rid (Rooms rooms) = do
  let roomID = RoomID rid
  room <- lookup roomID rooms
  pure (roomID, room)

-- | Get the current state of a room given its `RoomID`. Since we know the
-- `RoomID` is valid, this handles the "room does not exist" case by throwing an
-- exception.
readRoom :: RoomID -> TVar Rooms -> STM Room
readRoom roomID tvar = snd <$> readRoom' roomID tvar

readRoom' :: RoomID -> TVar Rooms -> STM (Rooms, Room)
readRoom' roomID tvar = do
  r@(Rooms rooms) <- readTVar tvar
  let room = fromMaybe (bug $ InvariantViolated $ "Room " <> show roomID <> " does not exist") $ lookup roomID rooms
  pure (r, room)

adjustRoom :: (Room -> Room) -> RoomID -> TVar Rooms -> STM Room
adjustRoom f roomID tvar = do
  (Rooms rooms, room) <- readRoom' roomID tvar
  let room' = f room
  writeTVar tvar $ Rooms $ insert roomID room' rooms
  pure room'

-- | The state of a single room.
data Room = Room
  { clients :: Map ClientID Client
  -- ^ The outbox for each client's websocket.
  , roomInbox :: TQueue (ClientID, ClientMessage)
  -- ^ The inbox for this room. Clients send their messages to the inbox of
  -- their room, along with a ClientID indicating who sent the message. This way
  -- the room manager can handle each message sequentially.
  , activeVideo :: Maybe ActiveVideo
  -- ^ The currently playing video.
  , queuedVideos :: [QueuedVideo]
  -- ^ A list of (client, video URL) pairs of queued videos.
  }

instance Show.Show Room where
  show Room{clients, activeVideo, queuedVideos} =
    "Room {clients = "
      <> show clients
      <> ", roomInbox = _"
      <> ", activeVideo = "
      <> show activeVideo
      <> ", queuedVideos = "
      <> show queuedVideos
      <> "}"

data Client = Client
  { outbox :: TQueue ServerMessage
  , handle :: Text
  }

instance Show.Show Client where
  show Client{handle} = "Client {outbox = _, handle = " <> show handle <> "}"

data ActiveVideo = Video
  { videoURL :: Text
  , submitter :: ClientID
  , playbackStatus :: PlaybackStatus
  , finishedClients :: Map ClientID Bool
  , skipVotes :: Map ClientID Bool
  }
  deriving stock (Show, Eq)

data PlaybackStatus
  = Playing {started :: UTCTime, fromSeekSeconds :: Int}
  | Paused {atSeekSeconds :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | The possible messages that can be sent by a client. Some of them are
-- synthetically constructed by the backend (e.g. the events for client
-- connection and disconnection).
data ClientMessage
  = -- | A client has joined. This event is synthetically generated on
    -- connection.
    ClientJoined
  | -- | A client has left. This event is synthetically generated on disconnect.
    ClientLeft
  | -- | A client wants to set its own handle.
    SetHandle {handle :: Text}
  | -- | A client wants to add a video to the queue.
    AddToQueue {videoURL :: Text}
  | RequestPlay {fromSeekSeconds :: Int}
  | RequestPause {atSeekSeconds :: Int}
  | PlaybackStarted
  | PlaybackFinished
  | Vote {skip :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | The possible messages that can be sent by the server.
data ServerMessage
  = UpdateClientList {clients :: [ClientListItem], you :: ClientListItem}
  | UpdateQueue {videos :: [QueuedVideo]}
  | UpdateVotes {skips :: Int}
  | SetPlayer {videoURL :: Text, playbackStatus :: PlaybackStatus, submitter :: ClientID}
  | UnsetPlayer
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ClientListItem = ClientListItem {clientID :: ClientID, handle :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

toClientList :: Map ClientID Client -> [ClientListItem]
toClientList clients = map (\(clientID, Client{handle}) -> ClientListItem{clientID, handle}) $ toPairs clients

data QueuedVideo = QueuedVideo {videoURL :: Text, submitter :: ClientID}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Allocate a room in the TVar, and then fork a thread that manages the room.
--
-- The room manager waits for new messages to this room's inbox, processes the
-- message, and then sends the replies to the room's clients' outboxes as
-- appropriate.
startRoom :: TVar Rooms -> IO RoomID
startRoom tvar = do
  roomID <- RoomID <$> liftIO generateID
  putStrLn $ "Starting new room " <> show roomID
  roomInbox <- atomically $ do
    roomInbox <- newTQueue
    let newRoom = Room{clients = mempty, roomInbox, activeVideo = Nothing, queuedVideos = []}
    modifyTVar' tvar $ \(Rooms rooms) -> Rooms $ insert roomID newRoom rooms
    pure roomInbox
  roomManager <- async $ roomLoop roomID roomInbox
  link roomManager
  pure roomID
 where
  -- TODO: Factor out the pure logic in this loop for QuickCheck testing.
  roomLoop :: RoomID -> TQueue (ClientID, ClientMessage) -> IO Void
  roomLoop roomID roomInbox = do
    putStrLn $ "Waiting for messages for room " <> show roomID
    msg <- atomically $ readTQueue roomInbox
    putStrLn $ "Got message for room " <> show roomID <> ": " <> show msg
    -- TODO: Refactor: make these branches all return `STM (Either String Room)`
    -- and log the `String` on failure or write the `Room` on success, and give
    -- the current time as an argument.
    room' <- case msg of
      -- Broadcast the new client list to all clients, and send the current
      -- playback state to the new client.
      --
      -- The client handler is responsible for having already updated the client
      -- list on join.
      (joined, ClientJoined) -> atomically $ do
        room <- readRoom roomID tvar
        let Client{outbox} =
              fromMaybe
                (bug $ InvariantViolated $ "ClientID " <> show joined <> " not found in joined room " <> show roomID <> ": " <> show room)
                $ lookup joined room.clients
        -- Send the updated client list to all clients.
        broadcastClientList room
        -- For the new client, send the current queue.
        unless (null room.queuedVideos) $ writeTQueue outbox $ UpdateQueue{videos = room.queuedVideos}
        -- For the new client, send the current playback state and votes.
        case room.activeVideo of
          Just video -> do
            writeTQueue outbox $ SetPlayer{videoURL = video.videoURL, playbackStatus = video.playbackStatus, submitter = video.submitter}
            writeTQueue outbox $ UpdateVotes{skips = length $ filter id $ elems video.skipVotes}
          Nothing -> pass
        pure room

      -- Broadcast the new client list to all clients. Clear all videos in the
      -- queue of the departing client. If there is an active video and the
      -- departing client is the submitter of the active video, go to the next
      -- remaining video. If there is an active video and the departing client
      -- is not the submitter, remove the departing client from the client
      -- finished status map.
      --
      -- The client handler is responsible for having already removed the client
      -- that left.
      (leaving, ClientLeft) -> do
        now <- getCurrentTime
        atomically $ do
          -- TODO: FIXME: When a client leaves, all of their videos in the queue
          -- should also be removed.
          (Rooms rooms, room@Room{activeVideo, queuedVideos}) <- readRoom' roomID tvar
          room' <- case activeVideo of
            Nothing -> pure room
            Just video -> do
              if video.submitter == leaving
                then
                  -- If the submitter left, move to the next video in the queue.
                  case filter ((/= leaving) . (.submitter)) queuedVideos of
                    -- If there are videos left, move to the next video and send UpdateQueue and SetPlayer.
                    QueuedVideo{videoURL, submitter} : queuedVideos' -> do
                      let room' =
                            room
                              { activeVideo =
                                  Just
                                    Video
                                      { videoURL
                                      , playbackStatus = Playing{fromSeekSeconds = 0, started = now}
                                      , submitter
                                      , finishedClients = mempty
                                      , skipVotes = mempty
                                      }
                              , queuedVideos = queuedVideos'
                              }
                      broadcast room' UpdateQueue{videos = queuedVideos'}
                      broadcast room' SetPlayer{videoURL, playbackStatus = Playing{fromSeekSeconds = 0, started = now}, submitter}
                      pure room'
                    -- If there are no videos left, clear the active video and send UnsetPlayer.
                    [] -> do
                      broadcast room UnsetPlayer
                      pure room{activeVideo = Nothing, queuedVideos = []}
                else do
                  -- Just remove the leaving client from the finished and votes maps.
                  let video' =
                        video
                          { finishedClients = delete leaving video.finishedClients
                          , skipVotes = delete leaving video.skipVotes
                          }
                  pure room{activeVideo = Just video'}
          broadcastClientList room'
          writeTVar tvar $ Rooms $ insert roomID room' rooms
          pure room'

      -- Update the client's handle, and then broadcast the new client list to
      -- all clients.
      (requestor, SetHandle handle') -> atomically $ do
        room' <-
          adjustRoom
            (\room -> room{clients = adjust (\client -> (client :: Client){handle = handle'}) requestor room.clients})
            roomID
            tvar
        broadcastClientList room'
        pure room'

      -- Add the video to the queue.
      --
      -- If the queue is empty and there is no active video, set the video to be
      -- the room's active video and broadcast the new playback state.
      --
      -- Otherwise, add the video to the queue and broadcast the new queue.
      (submitter, AddToQueue videoURL) -> do
        -- TODO: Validate the video URL before adding to queue.
        --
        -- TODO: Load information about the video via host API to display in the
        -- UI. See: https://developers.google.com/youtube/v3/docs/videos/list
        now <- getCurrentTime
        atomically $ do
          (Rooms rooms, room) <- readRoom' roomID tvar
          if null room.queuedVideos
            then case room.activeVideo of
              Nothing -> do
                -- If the queue is empty AND there is no active video, set the
                -- video to be the room's active video.
                let video =
                      Video
                        { videoURL
                        , submitter
                        , playbackStatus = Playing{started = now, fromSeekSeconds = 0}
                        , finishedClients = mempty
                        , skipVotes = mempty
                        }
                    room' = room{activeVideo = Just video}
                writeTVar tvar $ Rooms $ insert roomID room' rooms
                -- Broadcast the playback state update.
                broadcast room SetPlayer{videoURL = video.videoURL, playbackStatus = video.playbackStatus, submitter}
                pure room'
              -- If the queue is empty but there is an active video, add the
              -- video to the queue.
              Just _ -> addToQueueAndBroadcast rooms room
            else
              -- If the queue is not empty, add the video to the queue.
              addToQueueAndBroadcast rooms room
       where
        addToQueueAndBroadcast :: Map RoomID Room -> Room -> STM Room
        addToQueueAndBroadcast rooms room = do
          -- If the queue is not empty, add the video to the queue.
          let room' = room{queuedVideos = room.queuedVideos ++ [QueuedVideo{submitter, videoURL}]}
          writeTVar tvar $ Rooms $ insert roomID room' rooms
          -- Broadcast the queue update.
          broadcast room' UpdateQueue{videos = room'.queuedVideos}
          pure room'

      -- Check that there is a video ID set, and that the client is authorized
      -- to set the playback state. If so, update the room's playback status and
      -- broadcast the new player settings to all clients except the one that
      -- sent the play command (they are already playing).
      (origin, m@RequestPlay{fromSeekSeconds}) -> do
        now <- getCurrentTime
        withActiveVideoSubmitter origin m $ \(Rooms rooms) room video -> do
          let video' = (video :: ActiveVideo){playbackStatus = Playing{fromSeekSeconds, started = now}}
              room' = room{activeVideo = Just video'}
          writeTVar tvar $ Rooms $ insert roomID room' rooms
          broadcastExcept
            room'
            origin
            SetPlayer
              { videoURL = video'.videoURL
              , playbackStatus = video'.playbackStatus
              , submitter = video'.submitter
              }
          pure room'

      -- Check that there is a video ID set, and that the client is authorized
      -- to set the playback state. If so, update the room's playback status and
      -- broadcast the new player settings to all clients except the one that
      -- sent the stop command (they are already stopped).
      (origin, m@RequestPause{atSeekSeconds}) ->
        withActiveVideoSubmitter origin m $ \(Rooms rooms) room video -> do
          let video' = (video :: ActiveVideo){playbackStatus = Paused{atSeekSeconds}}
              room' = room{activeVideo = Just video'}
          writeTVar tvar $ Rooms $ insert roomID room' rooms
          broadcastExcept
            room'
            origin
            SetPlayer
              { videoURL = video'.videoURL
              , playbackStatus = video'.playbackStatus
              , submitter = video'.submitter
              }
          pure room'

      -- Update the finished clients of the active video, setting the watcher as
      -- unfinished.
      (watcher, m@PlaybackStarted) ->
        withActiveVideo watcher m $ \(Rooms rooms) room video -> do
          let room' = room{activeVideo = Just video{finishedClients = insert watcher False video.finishedClients}}
          writeTVar tvar $ Rooms $ insert roomID room' rooms
          pure room'

      -- Update the finished clients of the active video, setting the watcher as
      -- finished. If all clients have finished, move to the next video in the
      -- queue.
      (watcher, m@PlaybackFinished) -> do
        now <- getCurrentTime
        withActiveVideo watcher m $ \(Rooms rooms) room@Room{clients, queuedVideos} video@Video{finishedClients} -> do
          let finishedClients' = insert watcher True finishedClients
          room' <-
            if all (\clientID -> fromMaybe False $ lookup clientID finishedClients') $ keys clients
              then
                -- If everyone is done, move to the next video in the queue.
                --
                -- TODO: Factor out this action?
                case queuedVideos of
                  -- If there are videos left, move to the next video and send UpdateQueue and SetPlayer.
                  QueuedVideo{videoURL, submitter} : queuedVideos' -> do
                    let room' =
                          room
                            { activeVideo =
                                Just
                                  Video
                                    { videoURL
                                    , playbackStatus = Playing{fromSeekSeconds = 0, started = now}
                                    , submitter
                                    , finishedClients = mempty
                                    , skipVotes = mempty
                                    }
                            , queuedVideos = queuedVideos'
                            }
                    broadcast room' UpdateQueue{videos = queuedVideos'}
                    broadcast room' SetPlayer{videoURL, playbackStatus = Playing{fromSeekSeconds = 0, started = now}, submitter}
                    pure room'
                  -- If there are no videos left, clear the active video and send UnsetPlayer.
                  [] -> do
                    broadcast room UnsetPlayer
                    pure room{activeVideo = Nothing, queuedVideos = []}
              else do
                -- If not everyone is done, just set the watcher as finished.
                pure room{activeVideo = Just video{finishedClients = finishedClients'}}
          writeTVar tvar $ Rooms $ insert roomID room' rooms
          pure room'

      -- Update the skip votes of the active video. If the majority of
      -- non-submitter clients vote to skip, move to the next video in the
      -- queue.
      (voter, m@Vote{skip}) -> do
        now <- getCurrentTime
        withActiveVideo voter m $ \(Rooms rooms) room@Room{clients, queuedVideos} video@Video{skipVotes} -> do
          let skipVotes' = insert voter skip skipVotes
              -- TODO: Rather than filtering out the submitter vote from the
              -- ayes, we should prevent the submitter from submitting a vote in
              -- the first place, and log if they do so (since the UI normally
              -- does not allow this). This is too annoying to implement using
              -- the current callbacks, but definitely make sure to do this once
              -- this is refactored into `ExceptT` guard actions.
              ayes = length $ filter id $ elems $ delete video.submitter skipVotes'
          room' <-
            if ayes > ((size clients - 1) `div` 2)
              then
                -- With sufficient skips, move to the next video in the queue.
                case queuedVideos of
                  -- If there are videos left, move to the next video and send UpdateQueue and SetPlayer.
                  QueuedVideo{videoURL, submitter} : queuedVideos' -> do
                    let room' =
                          room
                            { activeVideo =
                                Just
                                  Video
                                    { videoURL
                                    , playbackStatus = Playing{fromSeekSeconds = 0, started = now}
                                    , submitter
                                    , finishedClients = mempty
                                    , skipVotes = mempty
                                    }
                            , queuedVideos = queuedVideos'
                            }
                    broadcast room' UpdateQueue{videos = queuedVideos'}
                    broadcast room' SetPlayer{videoURL, playbackStatus = Playing{fromSeekSeconds = 0, started = now}, submitter}
                    pure room'
                  -- If there are no videos left, clear the active video and send UnsetPlayer.
                  [] -> do
                    broadcast room UnsetPlayer
                    pure room{activeVideo = Nothing, queuedVideos = []}
              else do
                -- If not enough skips, just update the vote count and send
                -- UpdateVotes to inform the clients.
                broadcast room UpdateVotes{skips = ayes}
                pure room{activeVideo = Just video{skipVotes = skipVotes'}}
          writeTVar tvar $ Rooms $ insert roomID room' rooms
          pure room'

    putStrLn $ "Room state after handling message: " <> show room'
    roomLoop roomID roomInbox
   where
    broadcastSelect :: Room -> (ClientID -> Client -> Bool) -> (ClientID -> Client -> ServerMessage) -> STM ()
    broadcastSelect room select f = forM_
      (filter (uncurry select) $ toPairs room.clients)
      $ \(clientID, c@Client{outbox}) -> writeTQueue outbox $ f clientID c

    broadcast' :: Room -> (ClientID -> Client -> ServerMessage) -> STM ()
    broadcast' room = broadcastSelect room (\_ _ -> True)

    broadcast :: Room -> ServerMessage -> STM ()
    broadcast room msg = broadcast' room $ \_ _ -> msg

    broadcastExcept :: Room -> ClientID -> ServerMessage -> STM ()
    broadcastExcept room clientID msg = broadcastSelect room (\clientID' _ -> clientID' /= clientID) $ \_ _ -> msg

    broadcastClientList :: Room -> STM ()
    broadcastClientList room =
      broadcast' room $
        \clientID Client{handle} -> UpdateClientList{clients = toClientList room.clients, you = ClientListItem{clientID, handle}}

    withActiveVideo' :: ClientID -> ClientMessage -> (Rooms -> Room -> ActiveVideo -> STM (Either Room a)) -> IO (Either Room a)
    withActiveVideo' clientID msg f = do
      result <- atomically $ do
        (rooms, room) <- readRoom' roomID tvar
        case room.activeVideo of
          Nothing -> pure $ Left room
          Just video -> Right <$> f rooms room video
      case result of
        Right room' -> pure room'
        Left room -> do
          putStrLn $ "WARNING: Client " <> show clientID <> " sent " <> show msg <> ", but no active video in room " <> show roomID
          pure $ Left room

    withActiveVideo :: ClientID -> ClientMessage -> (Rooms -> Room -> ActiveVideo -> STM Room) -> IO Room
    withActiveVideo clientID msg f = do
      result <- withActiveVideo' clientID msg (\rooms room video -> Right <$> f rooms room video)
      case result of
        Left room -> pure room
        Right room -> pure room

    withActiveVideoSubmitter :: ClientID -> ClientMessage -> (Rooms -> Room -> ActiveVideo -> STM Room) -> IO Room
    withActiveVideoSubmitter clientID msg f = do
      result <- withActiveVideo' clientID msg $ \rooms room video -> do
        if video.submitter == clientID
          then Right <$> f rooms room video
          else pure $ Left room
      case result of
        Right room' -> pure room'
        Left room -> do
          putStrLn $ "WARNING: Client " <> show clientID <> " sent " <> show msg <> ", but was not submitter of active video in room " <> show roomID
          pure room

-- | Allocate a client in the TVar, and then fork a thread that manages the
-- client.
--
-- The client manager forks two threads. The first one listens to the client's
-- websocket, and sends any received messages to the room's inbox. The second
-- thread sends any messages from the room's outbox to the client's websocket.
startClient :: RoomID -> TVar Rooms -> Connection -> IO ()
startClient roomID tvar conn = do
  clientID <- ClientID <$> liftIO generateID
  putStrLn $ "Handling new client " <> show clientID
  (clientOutbox, roomInbox) <- atomically $ do
    clientOutbox <- newTQueue
    Room{roomInbox} <-
      adjustRoom
        (\room -> (room{clients = insert clientID Client{outbox = clientOutbox, handle = toText clientID} room.clients}))
        roomID
        tvar
    pure (clientOutbox, roomInbox)
  withPingPong defaultPingPongOptions conn $ \conn' -> do
    -- On join, send a message to the room to update the client list.
    sender <-
      async $
        atomically (writeTQueue roomInbox (clientID, ClientJoined))
          >> sendLoop conn' clientID clientOutbox
    receiver <-
      async $
        receiveLoop conn' clientID roomInbox
          `catch` handleGracefulDisconnect clientID roomInbox
          `catchAny` handleCrashDisconnect clientID roomInbox
    link2 sender receiver
    link receiver
    -- The receiver finishes once the client disconnects. After that happens,
    -- we need to clean up the spawned threads.
    wait receiver
    putStrLn $ "Cleaning up after client " <> show clientID <> " disconnected"
    cancel receiver
    cancel sender
    putStrLn $ "Cleaned up after client " <> show clientID <> " disconnected"
 where
  sendLoop :: Connection -> ClientID -> TQueue ServerMessage -> IO ()
  sendLoop conn' clientID clientOutbox = do
    putStrLn $ "Waiting for new message to send to " <> showCtx clientID
    msg <- atomically $ readTQueue clientOutbox
    putStrLn $ "Got new message to send to " <> showCtx clientID <> ": " <> show msg
    sendDataMessage conn' $ WS.Text (encode msg) Nothing
    putStrLn $ "Sent message to " <> showCtx clientID
    sendLoop conn' clientID clientOutbox

  receiveLoop :: Connection -> ClientID -> TQueue (ClientID, ClientMessage) -> IO ()
  receiveLoop conn' clientID roomInbox = do
    putStrLn $ "Waiting for new message from " <> showCtx clientID
    msg <- receiveDataMessage conn'
    putStrLn $ "Received message from " <> showCtx clientID <> ": " <> show msg
    msgData <- case msg of
      WS.Text msgData _ -> pure msgData
      WS.Binary _ -> bug $ InvariantViolated $ "Received binary message from " <> showCtx clientID <> ": " <> show msg
    let result = eitherDecode @ClientMessage msgData
    clientMessage <- case result of
      Left err -> bug $ InvariantViolated $ "Failed to parse message from " <> showCtx clientID <> ": " <> show err
      Right parsed -> pure parsed
    putStrLn $ "Parsed message from " <> showCtx clientID <> ": " <> show clientMessage
    atomically $ writeTQueue roomInbox (clientID, clientMessage)
    putStrLn $ "Wrote message to room inbox for " <> showCtx clientID
    receiveLoop conn' clientID roomInbox

  -- On disconnect, remove the client from the room's client list, and send a
  -- message to the room to update the client list.
  handleGracefulDisconnect :: ClientID -> TQueue (ClientID, ClientMessage) -> ConnectionException -> IO ()
  handleGracefulDisconnect clientID roomInbox exc = case exc of
    CloseRequest _ _ -> do
      putStrLn $ "Client " <> show clientID <> " gracefully disconnected"
      removeClient clientID roomInbox
      putStrLn $ "Client " <> show clientID <> " removed from client list after graceful disconnect"
    exc' -> throwIO exc'

  handleCrashDisconnect :: ClientID -> TQueue (ClientID, ClientMessage) -> SomeException -> IO ()
  handleCrashDisconnect clientID roomInbox exc = do
    putStrLn $ "WebSocket client " <> show clientID <> " is crashing: " <> displayException exc
    removeClient clientID roomInbox
    putStrLn $ "Client " <> show clientID <> " crashed and removed from client list"

  removeClient :: ClientID -> TQueue (ClientID, ClientMessage) -> IO ()
  removeClient clientID roomInbox = atomically $ do
    void $ adjustRoom (\room -> room{clients = delete clientID room.clients}) roomID tvar
    writeTQueue roomInbox (clientID, ClientLeft)

  showCtx :: ClientID -> String
  showCtx clientID = "client " <> show clientID <> " in room " <> show roomID

-- | A counter used to generate unique IDs.
nextID :: IORef Int
{-# NOINLINE nextID #-}
nextID = unsafePerformIO $ newIORef 0

-- | Generate a new unique ID. This synchronizes between different handler
-- threads, so may become a performance bottleneck.
generateID :: IO Text
generateID = do
  nextID' <- atomicModifyIORef' nextID $ \n -> let n' = n + 1 in (n', n')
  case runSqids defaultSqidsOptions{minLength = 6} $ Sqids.encode [nextID'] of
    Left err -> bug $ Impossible $ "sqid generation failed: " <> show err
    Right generated -> pure generated

-- | An exception for use with `bug`.
data BugException = InvariantViolated String | Impossible String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Like `threadDelay`, but takes input in seconds.
_delaySeconds :: Int -> IO ()
_delaySeconds = threadDelay . (* 1000000)
