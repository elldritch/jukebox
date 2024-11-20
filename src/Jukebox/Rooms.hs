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
import Relude.Extra.Map (delete, insert, lookup, toPairs)

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | The possible messages that can be sent by the server.
data ServerMessage
  = UpdateClientList {clients :: [ClientListItem], you :: ClientListItem}
  | UpdateQueue {videos :: [QueuedVideo]}
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
    room' <- case msg of
      -- Broadcast the new client list to all clients, and send the current
      -- playback state to the new client. The client handler is responsible for
      -- having already updated the client list on join.
      (joined, ClientJoined) ->
        atomically $ do
          room <- readRoom roomID tvar
          let Client{outbox} =
                fromMaybe
                  (bug $ InvariantViolated $ "ClientID " <> show joined <> " not found in joined room " <> show roomID <> ": " <> show room)
                  $ lookup joined room.clients
          -- Send the updated client list to all clients.
          broadcastClientList room
          -- For the new client, send the current queue.
          unless (null room.queuedVideos) $ writeTQueue outbox $ UpdateQueue{videos = room.queuedVideos}
          -- For the new client, send the current playback state.
          case room.activeVideo of
            Just video -> writeTQueue outbox $ SetPlayer{videoURL = video.videoURL, playbackStatus = video.playbackStatus, submitter = video.submitter}
            Nothing -> pass
          pure room

      -- Broadcast the new client list to all clients. The client handler is
      -- responsible for having already removed the client that left.
      (_, ClientLeft) ->
        atomically $ do
          room <- readRoom roomID tvar
          broadcastClientList room
          pure room

      -- Update the client's handle, and then broadcast the new client list to
      -- all clients.
      (requestor, SetHandle handle') ->
        atomically $ do
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
                        }
                    room' = room{activeVideo = Just video}
                writeTVar tvar $ Rooms $ insert roomID room' rooms
                -- Broadcast the playback state update.
                broadcastMessage room SetPlayer{videoURL = video.videoURL, playbackStatus = video.playbackStatus, submitter}
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
          broadcastMessage room' UpdateQueue{videos = room'.queuedVideos}
          pure room'

      -- Check that there is a video ID set. If so, update the room's playback
      -- status and broadcast the new player settings to all clients except the
      -- one that sent the play command (they are already playing).
      (origin, RequestPlay{fromSeekSeconds}) -> undefined
      --   atomically $ do
      --     r@Room{clients, playbackStatus} <- modifyRoom (\r -> r{playbackStatus = Playing}) roomID tvar
      --     case r.videoID of
      --       Just videoID -> do
      --         let outboxes = fmap ((.outbox) . snd) $ filter ((/= origin) . fst) $ toPairs clients
      --         forM_ outboxes $ \outbox -> writeTQueue outbox SetPlayer{videoID, playbackStatus}
      --       Nothing -> pass
      --     pure r

      -- Check that there is a video ID set. If so, update the room's playback
      -- status and broadcast the new player settings to all clients except the
      -- one that sent the stop command (they are already stopped).
      (origin, RequestPause{atSeekSeconds}) -> undefined
    --   atomically $ do
    --     r@Room{clients, playbackStatus} <- modifyRoom (\r -> r{playbackStatus = Stopped}) roomID tvar
    --     case r.videoID of
    --       Just videoID -> do
    --         let outboxes = fmap ((.outbox) . snd) $ filter ((/= origin) . fst) $ toPairs clients
    --         forM_ outboxes $ \outbox -> writeTQueue outbox SetPlayer{videoID, playbackStatus}
    --       Nothing -> pass
    --     pure r
    putStrLn $ "Room state after handling message: " <> show room'
    roomLoop roomID roomInbox
   where
    broadcastMessage :: Room -> ServerMessage -> STM ()
    broadcastMessage room msg = broadcastMessage' room $ \_ _ -> msg

    broadcastMessage' :: Room -> (ClientID -> Client -> ServerMessage) -> STM ()
    broadcastMessage' room f = forM_ (toPairs room.clients) $ \(clientID, c@Client{outbox}) -> writeTQueue outbox $ f clientID c

    broadcastClientList :: Room -> STM ()
    broadcastClientList room =
      broadcastMessage' room $
        \clientID Client{handle} -> UpdateClientList{clients = toClientList room.clients, you = ClientListItem{clientID, handle}}

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
    putStrLn $ "Cleaning up after client" <> show clientID <> " disconnected"
    cancel receiver
    cancel sender
    putStrLn $ "Cleaned up after client" <> show clientID <> " disconnected"
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
