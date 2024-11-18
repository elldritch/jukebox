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
import Relude.Extra.Map (delete, insert, keys, lookup, toPairs)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, link, wait)
import Control.Concurrent.STM (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Exception.Safe (catch, throwIO)
import Data.Aeson (ToJSON (..), eitherDecode, withObject)
import Data.Aeson.Types (Parser, Value, parseEither, parseFail, (.:))
import Network.WebSockets (
  Connection,
  ConnectionException (..),
  defaultPingPongOptions,
  receiveDataMessage,
  sendDataMessage,
  withPingPong,
 )
import Network.WebSockets qualified as WS
import Servant (FromHttpApiData)
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html ((!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (li, toHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
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

modifyRoom :: (Room -> (Room, a)) -> RoomID -> TVar Rooms -> STM a
modifyRoom f roomID tvar = do
  (Rooms rooms, room) <- readRoom' roomID tvar
  let (room', a) = f room
  writeTVar tvar $ Rooms $ insert roomID room' rooms
  pure a

modifyRoom_ :: (Room -> Room) -> RoomID -> TVar Rooms -> STM ()
modifyRoom_ f = modifyRoom $ \r -> (f r, ())

-- | The state of a single room.
data Room = Room
  { clientOutboxes :: Map ClientID (TQueue ServerMessage)
  -- ^ The outbox for each client's websocket.
  , roomInbox :: TQueue (ClientID, ClientMessage)
  -- ^ The inbox for this room. Clients send their messages to the inbox of
  -- their room, along with a ClientID indicating who sent the message. This way
  -- the room manager can handle each message sequentially.
  }

-- | The possible messages that can be sent by a client. These are generally
-- sent via HTMX's ws-send on the client side, and so are parsed from JSON
-- messages. Some of them are synthetically constructed by the backend (e.g.
-- when clients connect or disconnect).
data ClientMessage
  = ClientListChanged
  | RequestVideoID {videoID :: Text}
  deriving stock (Show, Eq)

-- | Parse JSON client messages sent via WebSocket. These messages are all
-- structured to have an @action@ field that is used as a discriminator.
parseClientMessage :: Value -> Parser ClientMessage
parseClientMessage = withObject "ClientMessage" $ \o -> do
  action :: Text <- o .: "action"
  case action of
    "set-video-id" -> do
      videoID <- o .: "video-id"
      pure $ RequestVideoID{videoID}
    _ -> parseFail $ "invalid ClientMessage action: " <> show action

-- | The possible messages that can be sent by the server. These are generally
-- sent as HTML for HTMX to pick up and render. Some message types have special
-- handling in JavaScript on the client side since they're used to trigger UI
-- actions in the player.
data ServerMessage
  = UpdateClientList {clients :: [ClientID], you :: ClientID}
  | SetVideoID {videoID :: Text}
  | Play
  | Pause
  | Seek {seconds :: Double}
  deriving stock (Show, Eq, Generic)

-- | Render a message for sending to the client.
renderServerMessage :: ServerMessage -> LByteString
renderServerMessage UpdateClientList{clients, you = ClientID you} =
  renderHtml $ H.div ! A.id "listeners" $ do
    forM_ clients $ \(ClientID cid) -> do
      li $ toHtml (cid <> if cid == you then " (you)" else "")
renderServerMessage _ = undefined

-- | Allocate a room in the TVar, and then fork a thread that manages the room.
--
-- The room manager waits for new messages to this room's inbox, handles the
-- message and constructs reply messages if needed, and then sends the replies
-- to the room's clients' outboxes.
startRoom :: TVar Rooms -> IO RoomID
startRoom tvar = do
  roomID <- RoomID <$> liftIO generateID
  putStrLn $ "Starting new room " <> show roomID
  roomInbox <- atomically $ do
    roomInbox <- newTQueue
    modifyTVar' tvar $ \(Rooms rooms) -> Rooms $ insert roomID Room{clientOutboxes = mempty, roomInbox} rooms
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
    case msg of
      (_, ClientListChanged) -> do
        atomically $ do
          room <- readRoom roomID tvar
          forM_ (toPairs room.clientOutboxes) $ \(clientID, outbox) -> do
            writeTQueue outbox $ UpdateClientList{clients = keys room.clientOutboxes, you = clientID}
      (_, RequestVideoID _) -> undefined
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
    roomInbox <-
      modifyRoom
        (\room -> (room{clientOutboxes = insert clientID clientOutbox room.clientOutboxes}, room.roomInbox))
        roomID
        tvar
    pure (clientOutbox, roomInbox)
  withPingPong defaultPingPongOptions conn $ \conn' -> do
    -- On join, send a message to the room to update the client list.
    sender <-
      async $
        atomically (writeTQueue roomInbox (clientID, ClientListChanged))
          >> sendLoop conn' clientID clientOutbox
    receiver <-
      async $
        receiveLoop conn' clientID roomInbox
          `catch` handleDisconnect clientID roomInbox
    link sender
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
    sendDataMessage conn' $ WS.Text (renderServerMessage msg) Nothing
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
    let result = eitherDecode msgData >>= parseEither parseClientMessage
    clientMessage <- case result of
      Left err -> bug $ InvariantViolated $ "Failed to parse message from " <> showCtx clientID <> ": " <> show err
      Right parsed -> pure parsed
    putStrLn $ "Parsed message from " <> showCtx clientID <> ": " <> show clientMessage
    atomically $ writeTQueue roomInbox (clientID, clientMessage)
    putStrLn $ "Wrote message to room supervisor for " <> showCtx clientID
    receiveLoop conn' clientID roomInbox

  -- On disconnect, remove the client from the room's client list, and send a
  -- message to the room to update the client list.
  handleDisconnect :: ClientID -> TQueue (ClientID, ClientMessage) -> ConnectionException -> IO ()
  handleDisconnect clientID roomInbox exc = case exc of
    CloseRequest _ _ -> do
      putStrLn $ "Client " <> show clientID <> " disconnected"
      atomically $ do
        modifyRoom_ (\room -> room{clientOutboxes = delete clientID room.clientOutboxes}) roomID tvar
        writeTQueue roomInbox (clientID, ClientListChanged)
    exc' -> throwIO exc'

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
