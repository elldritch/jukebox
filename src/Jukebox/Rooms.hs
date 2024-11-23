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

import Control.Concurrent.Async (async, cancel, link, link2, wait)
import Control.Concurrent.STM (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Exception.Safe (catch, catchAny, throwIO)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, eitherDecode, encode)
import Data.Map.Strict (adjust)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Conc (unsafeIOToSTM)
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

adjustRoom :: (Room -> Room) -> RoomID -> TVar Rooms -> STM Room
adjustRoom f roomID tvar = do
  (Rooms rooms) <- readTVar tvar
  let room = fromMaybe (bug $ InvariantViolated $ "Room " <> show roomID <> " does not exist") $ lookup roomID rooms
      room' = f room
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
  { videoID :: Text
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
    AddToQueue {videoID :: Text}
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
  | SetPlayer {videoID :: Text, playbackStatus :: PlaybackStatus, submitter :: ClientID}
  | UnsetPlayer
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ClientListItem = ClientListItem {clientID :: ClientID, handle :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

toClientList :: Map ClientID Client -> [ClientListItem]
toClientList clients = map (\(clientID, Client{handle}) -> ClientListItem{clientID, handle}) $ toPairs clients

data QueuedVideo = QueuedVideo {videoID :: Text, submitter :: ClientID}
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
  roomLoop :: RoomID -> TQueue (ClientID, ClientMessage) -> IO Void
  roomLoop roomID roomInbox = do
    putStrLn $ "Waiting for messages for room " <> show roomID
    (sender, msg) <- atomically $ readTQueue roomInbox
    putStrLn $ "Got message for room " <> show roomID <> " from " <> show sender <> ": " <> show (sender, msg)
    result <- atomically $ do
      Rooms rooms <- readTVar tvar
      let room = fromMaybe (bug $ InvariantViolated $ "Room " <> show roomID <> " does not exist") $ lookup roomID rooms
      result <- (Right <$> handleMessage room (sender, msg)) `catchSTM` (\(err :: BugException) -> pure $ Left err)
      case result of
        Right room' -> Right room' <$ writeTVar tvar (Rooms $ insert roomID room' rooms)
        Left err -> pure $ Left err
    case result of
      Left err -> putStrLn $ "Error handling message for room " <> show roomID <> " from client " <> show sender <> ": " <> displayException err
      Right room' -> putStrLn $ "Room state after handling message: " <> show room'
    roomLoop roomID roomInbox

-- TODO: Add QuickCheck tests.
handleMessage :: Room -> (ClientID, ClientMessage) -> STM Room
handleMessage room = \case
  -- Broadcast the new client list to all clients. Send the current playback
  -- state, video queue, and votes to the new client.
  --
  -- The receiving thread is responsible for having already updated the client
  -- list on join.
  (joined, ClientJoined) -> do
    -- Broadcast the new client list to all clients.
    broadcastClientList
    -- Send the current playback state, video queue, and votes to the new client.
    room <$ case room.activeVideo of
      Just video -> do
        sendTo joined $ UpdateQueue{videos = room.queuedVideos}
        sendTo joined $ SetPlayer{videoID = video.videoID, playbackStatus = video.playbackStatus, submitter = video.submitter}
        sendTo joined $ UpdateVotes{skips = ayes video.skipVotes}
      Nothing -> pass

  -- Broadcast the new client list to all clients. Clear all videos in the queue
  -- submitted by the departing client. If there is an active video and the
  -- departing client is the submitter of the active video, go to the next
  -- remaining video. If there is an active video and the departing client is
  -- not the submitter, remove the departing client's finished status and votes.
  --
  -- The receiving thread is responsible for having already removed the client
  -- that left.
  (leaving, ClientLeft) -> do
    -- Broadcast the new client list to all clients.
    broadcastClientList
    -- Clear all videos in the queue submitted by the departing client.
    let room' = room{queuedVideos = filter ((/= leaving) . (.submitter)) room.queuedVideos}
    case room'.activeVideo of
      Just video ->
        if video.submitter == leaving
          -- If there is an active video and the departing client is the
          -- submitter, go to the next remaining video.
          then playNextVideo room'
          -- If there is an active video and the departing client is not the
          -- submitter, remove the departing client's finished status and votes.
          else do
            let video' = video{finishedClients = delete leaving video.finishedClients, skipVotes = delete leaving video.skipVotes}
            broadcast $ UpdateVotes{skips = ayes video'.skipVotes}
            pure $ room'{activeVideo = Just video'}
      Nothing -> pure room'

  -- Update the client's handle, and then broadcast the new client list to
  -- all clients.
  (clientID, SetHandle{handle}) -> do
    let room' = (room :: Room){clients = adjust (\c -> (c :: Client){handle}) clientID room.clients}
    room' <$ broadcastClientList' room'.clients

  -- If the room has no active video, set the video to be the room's active
  -- video and broadcast the new playback state. Otherwise, update the room's
  -- queue and broadcast the new queue state.
  (submitter, AddToQueue{videoID}) -> case room.activeVideo of
    -- If there is no active video, this implies that the queue is empty, so we
    -- don't need to worry about checking or updating the queue.
    Nothing -> do
      video <- playVideo QueuedVideo{submitter, videoID}
      pure $ room{activeVideo = Just video}
    Just _ -> do
      let room' = room{queuedVideos = room.queuedVideos ++ [QueuedVideo{submitter, videoID}]}
      room' <$ broadcast UpdateQueue{videos = room'.queuedVideos}

  -- Check that there is a video ID set, and that the client is authorized
  -- to set the playback state. If so, update the room's playback status and
  -- broadcast the new player settings to all clients except the one that
  -- sent the play command (they are already playing).
  (clientID, RequestPlay{fromSeekSeconds}) -> do
    activeVideo <- requireActiveVideoSubmitter clientID
    now <- getNow
    let video' = (activeVideo :: ActiveVideo){playbackStatus = Playing{fromSeekSeconds, started = now}}
        room' = room{activeVideo = Just video'}
    room' <$ broadcastExcept clientID SetPlayer{videoID = video'.videoID, playbackStatus = video'.playbackStatus, submitter = video'.submitter}

  -- Check that there is a video ID set, and that the client is authorized
  -- to set the playback state. If so, update the room's playback status and
  -- broadcast the new player settings to all clients except the one that
  -- sent the pause command (they are already paused).
  (clientID, RequestPause{atSeekSeconds}) -> do
    activeVideo <- requireActiveVideoSubmitter clientID
    let video' = (activeVideo :: ActiveVideo){playbackStatus = Paused{atSeekSeconds}}
        room' = room{activeVideo = Just video'}
    room' <$ broadcastExcept clientID SetPlayer{videoID = video'.videoID, playbackStatus = video'.playbackStatus, submitter = video'.submitter}

  -- Update the finished clients of the active video, setting the watcher as
  -- unfinished.
  (watcher, PlaybackStarted) -> do
    activeVideo <- requireActiveVideo
    let video' = activeVideo{finishedClients = insert watcher False activeVideo.finishedClients}
    pure room{activeVideo = Just video'}

  -- Update the finished clients of the active video, setting the watcher as
  -- finished. If all clients have finished, move to the next video in the
  -- queue.
  (watcher, PlaybackFinished) -> do
    activeVideo <- requireActiveVideo
    let finishedClients' = insert watcher True activeVideo.finishedClients
        room' = room{activeVideo = Just activeVideo{finishedClients = finishedClients'}}
    if all (\clientID -> fromMaybe False $ lookup clientID finishedClients') $ keys room.clients
      then playNextVideo room
      else pure room'

  -- Update the skip votes of the active video. If the majority of non-submitter
  -- clients vote to skip, move to the next video in the queue.
  (voter, Vote{skip}) -> do
    activeVideo <- requireActiveVideo
    let skipVotes' = insert voter skip activeVideo.skipVotes
        room' = room{activeVideo = Just activeVideo{skipVotes = skipVotes'}}
    if ayes skipVotes' > (size room.clients - 1) `div` 2
      then playNextVideo room'
      else room' <$ broadcast UpdateVotes{skips = ayes skipVotes'}
 where
  broadcastSelect :: ((ClientID, Client) -> Bool) -> ((ClientID, Client) -> ServerMessage) -> STM ()
  broadcastSelect select f = forM_
    (filter select $ toPairs room.clients)
    $ \(clientID, c@Client{outbox}) -> writeTQueue outbox $ f (clientID, c)

  sendTo :: ClientID -> ServerMessage -> STM ()
  sendTo clientID msg = broadcastSelect ((== clientID) . fst) $ const msg

  broadcastExcept :: ClientID -> ServerMessage -> STM ()
  broadcastExcept clientID msg = broadcastSelect ((/= clientID) . fst) $ const msg

  broadcast' :: ((ClientID, Client) -> ServerMessage) -> STM ()
  broadcast' = broadcastSelect $ const True

  broadcastClientList :: STM ()
  broadcastClientList = broadcastClientList' room.clients

  -- This takes an explicit `clients` argument instead of using `room.clients`
  -- in case the caller wants to change the client's handle. Notice that we
  -- still use the original `room.clients`'s outboxes. This is safe because
  -- `handleMessage` should never be adding or removing clients, and should
  -- never be altering the outboxes of clients.
  broadcastClientList' :: Map ClientID Client -> STM ()
  broadcastClientList' clients =
    broadcast' $ \(clientID, Client{handle}) -> UpdateClientList{clients = toClientList clients, you = ClientListItem{clientID, handle}}

  broadcast :: ServerMessage -> STM ()
  broadcast msg = broadcast' $ const msg

  ayes :: Map ClientID Bool -> Int
  ayes = length . filter id . elems

  getNow :: STM UTCTime
  getNow = unsafeIOToSTM getCurrentTime

  playVideo :: QueuedVideo -> STM ActiveVideo
  playVideo QueuedVideo{videoID, submitter} = do
    now <- getNow
    let playbackStatus = Playing{fromSeekSeconds = 0, started = now}
        video = Video{videoID, playbackStatus, submitter, skipVotes = mempty, finishedClients = mempty}
    video <$ broadcast SetPlayer{videoID, playbackStatus, submitter}

  playNextVideo :: Room -> STM Room
  playNextVideo r = case r.queuedVideos of
    nextInQueue : queueTail -> do
      nextVideo <- playVideo nextInQueue
      broadcast UpdateQueue{videos = queueTail}
      pure r{activeVideo = Just nextVideo, queuedVideos = queueTail}
    [] -> r{activeVideo = Nothing, queuedVideos = []} <$ broadcast UnsetPlayer

  requireActiveVideo :: STM ActiveVideo
  requireActiveVideo = case room.activeVideo of
    Just video -> pure video
    Nothing -> throwSTM $ InvariantViolated "no active video in room"

  requireActiveVideoSubmitter :: ClientID -> STM ActiveVideo
  requireActiveVideoSubmitter client = do
    video <- requireActiveVideo
    if video.submitter == client then pure video else throwSTM $ InvariantViolated "client is not submitter of active video"

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
