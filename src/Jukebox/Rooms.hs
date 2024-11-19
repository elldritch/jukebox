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
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Map.Strict (adjust)
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
  deriving newtype (Eq, Ord, FromHttpApiData, ToText, ToJSON)

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

modifyRoom :: (Room -> Room) -> RoomID -> TVar Rooms -> STM Room
modifyRoom f roomID tvar = do
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
  , videoID :: Maybe Text
  -- ^ The ID of the currently playing video.
  , playbackStatus :: PlaybackStatus
  -- ^ The playback status of the currently playing video.
  }

instance Show.Show Room where
  show Room{clients, videoID, playbackStatus} =
    "Room {clientOutboxes = "
      <> show clients
      <> ", roomInbox = _"
      <> ", videoID = "
      <> show videoID
      <> ", playbackStatus = "
      <> show playbackStatus
      <> "}"

data PlaybackStatus = Playing | Stopped
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data Client = Client
  { outbox :: TQueue ServerMessage
  , handle :: Text
  }

instance Show.Show Client where
  show Client{handle} = "Client {outbox = _, handle = " <> show handle <> "}"

-- | The possible messages that can be sent by a client. Some of them are
-- synthetically constructed by the backend (e.g. the events for client
-- connection and disconnection).
data ClientMessage
  = -- | A client has joined. This event is synthetically generated on
    -- connection.
    ClientJoined
  | -- | A client has left. This event is synthetically generated on disconnect.
    ClientLeft
  | RequestVideoID {videoID :: Text}
  | RequestPlay
  | RequestStop
  | SetHandle {handle :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | The possible messages that can be sent by the server.
data ServerMessage
  = UpdateClientList {clients :: [ClientListItem], you :: ClientListItem}
  | SetPlayer {videoID :: Text, playbackStatus :: PlaybackStatus}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ClientListItem = ClientListItem
  { clientID :: ClientID
  , handle :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

toClientList :: Map ClientID Client -> [ClientListItem]
toClientList clients = map (\(clientID, Client{handle}) -> ClientListItem{clientID, handle}) $ toPairs clients

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
    let newRoom = Room{clients = mempty, roomInbox, videoID = Nothing, playbackStatus = Stopped}
    modifyTVar' tvar $ \(Rooms rooms) -> Rooms $ insert roomID newRoom rooms
    pure roomInbox
  roomManager <- async $ roomLoop roomID roomInbox
  link roomManager
  pure roomID
 where
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
          -- Send the updated client list to all clients.
          forM_ (toPairs room.clients) $ \(clientID, Client{outbox, handle}) ->
            writeTQueue outbox $ UpdateClientList{clients = toClientList room.clients, you = ClientListItem{clientID, handle}}
          -- TODO: For the new client, update their queue and player.
          pure room

      -- Broadcast the new client list to all clients. The client handler is
      -- responsible for having already updated the client list on leave.
      (_, ClientLeft) ->
        atomically $ do
          room <- readRoom roomID tvar
          forM_ (toPairs room.clients) $ \(clientID, Client{outbox, handle}) ->
            writeTQueue outbox $ UpdateClientList{clients = toClientList room.clients, you = ClientListItem{clientID, handle}}
          pure room

      -- Update the client's handle, and then broadcast the new client list to
      -- all clients.
      (requestor, SetHandle handle') ->
        atomically $ do
          room <- modifyRoom (\room -> room{clients = adjust (\client -> (client :: Client){handle = handle'}) requestor room.clients}) roomID tvar
          forM_ (toPairs room.clients) $ \(clientID, Client{outbox, handle}) ->
            writeTQueue outbox $ UpdateClientList{clients = toClientList room.clients, you = ClientListItem{clientID, handle}}
          pure room

      -- Update the room's video ID and playback status and broadcast the new
      -- player settings to all clients.
      (_, RequestVideoID videoID) ->
        atomically $ do
          r@Room{clients, playbackStatus} <- modifyRoom (\r -> r{videoID = Just videoID, playbackStatus = Stopped}) roomID tvar
          forM_ clients $ \Client{outbox} -> writeTQueue outbox $ SetPlayer{videoID, playbackStatus}
          pure r

      -- Check that there is a video ID set. If so, update the room's playback
      -- status and broadcast the new player settings to all clients except the
      -- one that sent the play command (they are already playing).
      (origin, RequestPlay) ->
        atomically $ do
          r@Room{clients, playbackStatus} <- modifyRoom (\r -> r{playbackStatus = Playing}) roomID tvar
          case r.videoID of
            Just videoID -> do
              let outboxes = fmap ((.outbox) . snd) $ filter ((/= origin) . fst) $ toPairs clients
              forM_ outboxes $ \outbox -> writeTQueue outbox SetPlayer{videoID, playbackStatus}
            Nothing -> pass
          pure r

      -- Check that there is a video ID set. If so, update the room's playback
      -- status and broadcast the new player settings to all clients except the
      -- one that sent the stop command (they are already stopped).
      (origin, RequestStop) ->
        atomically $ do
          r@Room{clients, playbackStatus} <- modifyRoom (\r -> r{playbackStatus = Stopped}) roomID tvar
          case r.videoID of
            Just videoID -> do
              let outboxes = fmap ((.outbox) . snd) $ filter ((/= origin) . fst) $ toPairs clients
              forM_ outboxes $ \outbox -> writeTQueue outbox SetPlayer{videoID, playbackStatus}
            Nothing -> pass
          pure r
    putStrLn $ "Room state after handling message: " <> show room'
    roomLoop roomID roomInbox

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
      modifyRoom
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
    void $ modifyRoom (\room -> room{clients = delete clientID room.clients}) roomID tvar
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
