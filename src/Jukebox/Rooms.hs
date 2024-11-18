module Jukebox.Rooms (
  RoomID (..),
  ClientID (..),
  Rooms (..),
  Room (..),
  startRoom,
  handleClient,
) where

import Relude
import Relude.Extra.Map (delete, insert, keys, lookup, toPairs)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, link, wait)
import Control.Concurrent.STM (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Exception.Safe (catch, throwIO)
import Data.Aeson (ToJSON (..), decode, defaultOptions, eitherDecode, encode, genericToEncoding, withObject)
import Data.Aeson.Types (Parser, Value, parseEither, parseFail, (.:))
import Data.Default (Default (..))
import Network.WebSockets (
  Connection,
  ConnectionException (..),
  DataMessage (..),
  defaultPingPongOptions,
  receiveDataMessage,
  sendDataMessage,
  withPingPong,
 )
import Network.WebSockets qualified as WS
import Servant (FromHttpApiData)
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html (Html, ToMarkup, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (li, p, toHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Sqids (SqidsOptions (..), defaultSqidsOptions, runSqids)
import Web.Sqids qualified as Sqids

newtype RoomID = RoomID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromHttpApiData, ToString, ToMarkup, ToJSON)

instance ConvertUtf8 RoomID ByteString where
  encodeUtf8 = encodeUtf8 . coerce @RoomID @Text
  decodeUtf8 = coerce @Text @RoomID . decodeUtf8
  decodeUtf8Strict = (coerce @Text @RoomID <$>) . decodeUtf8Strict

newtype ClientID = ClientID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromHttpApiData, ToJSON)

newtype Rooms = Rooms {rooms :: Map RoomID Room}

instance Default Rooms where
  def = Rooms mempty

-- Each room has one thread, and each websocket has two threads.
--
-- The websocket threads:
-- 1. Receiving messages from the client, and putting them into the inbox of the
--    client's room.
-- 2. Waiting for messages in the client's outbox, and sending them to the
--    client.
--
-- The room thread receives messages from the room's inbox, acts on them, and
-- then puts any new messages into the outboxes of the correct clients.
data Room = Room
  { clientOutboxes :: Map ClientID (TQueue ServerMessage)
  , roomInbox :: TQueue ClientMessage
  , owner :: Maybe ClientID
  }

-- Allocate a room in the TVar, and then fork a thread that manages the room.
startRoom :: TVar Rooms -> IO (RoomID, Async Void)
startRoom tvar = do
  roomID <- RoomID <$> liftIO generateID
  roomInbox <- atomically $ do
    roomInbox <- newTQueue
    Rooms{rooms} <- readTVar tvar
    let newRoom = Room{clientOutboxes = mempty, roomInbox, owner = Nothing}
        rooms' = Rooms{rooms = insert roomID newRoom rooms}
    writeTVar tvar rooms'
    pure roomInbox
  _debug <- async $ debugLoop roomID
  roomManager <- async $ roomThread roomID roomInbox
  link roomManager
  pure (roomID, roomManager)
 where
  debugLoop :: RoomID -> IO ()
  debugLoop roomID = putStrLn ("Heartbeat: room " <> show roomID) >> delaySeconds 10 >> debugLoop roomID

  roomThread :: RoomID -> TQueue ClientMessage -> IO Void
  roomThread roomID roomInbox = do
    putStrLn $ "Waiting for messages to room " <> show roomID
    msg <- atomically $ readTQueue roomInbox
    putStrLn $ "Got message for room " <> show roomID <> ": " <> show msg
    case msg of
      ClientListChanged -> do
        atomically $ do
          room <- readRoom roomID id tvar
          forM_ (toPairs room.clientOutboxes) $ \(clientID, outbox) -> do
            writeTQueue outbox $
              UpdateClientList
                { clients = keys room.clientOutboxes
                , owner = room.owner
                , you = clientID
                }
      RequestVideoID videoID -> undefined
    roomThread roomID roomInbox

data ServerMessage
  = UpdateClientList {clients :: [ClientID], owner :: Maybe ClientID, you :: ClientID}
  | SetVideoURL {url :: Text}
  | Play
  | Pause
  | Seek {seconds :: Double}
  deriving stock (Show, Eq, Generic)

renderServerMessage :: ServerMessage -> Html
renderServerMessage UpdateClientList{clients, owner, you = ClientID you} =
  H.div ! A.id "listeners" $ do
    forM_ clients $ \(ClientID cid) -> do
      li $ toHtml (cid <> if cid == you then " (you)" else "")
renderServerMessage _ = undefined

instance ToJSON ServerMessage where
  toEncoding = genericToEncoding defaultOptions

data ClientMessage
  = ClientListChanged
  | ClientLeft
  | RequestVideoID {videoID :: Text}
  deriving stock (Show, Eq)

withRoom :: RoomID -> (Room -> (Room, a)) -> TVar Rooms -> STM a
withRoom roomID f tvar = do
  r@Rooms{rooms} <- readTVar tvar
  let (newRoom, projected) = f $ fromMaybe (bug $ InvariantViolated $ "Room " <> show roomID <> " does not exist") $ lookup roomID rooms
  writeTVar tvar r{rooms = insert roomID newRoom rooms}
  pure projected

readRoom :: RoomID -> (Room -> a) -> TVar Rooms -> STM a
readRoom roomID f = withRoom roomID (\room -> (room, f room))

modifyRoom :: RoomID -> (Room -> Room) -> TVar Rooms -> STM ()
modifyRoom roomID f = withRoom roomID (\room -> (f room, ()))

handleClient :: RoomID -> TVar Rooms -> Connection -> IO ()
handleClient roomID tvar conn = do
  clientID <- ClientID <$> liftIO generateID
  (clientOutbox, roomInbox) <- atomically $ do
    clientOutbox <- newTQueue
    roomInbox <-
      withRoom
        roomID
        ( \room ->
            ( room{clientOutboxes = insert clientID clientOutbox room.clientOutboxes}
            , room.roomInbox
            )
        )
        tvar
    pure (clientOutbox, roomInbox)
  withPingPong defaultPingPongOptions conn $ \conn' -> do
    _debug <- async $ debugLoop clientID
    _sender <- async $ do
      -- Queue initial messages for room broadcasting.
      putStrLn $ "Queueing initial messages on client join for " <> show clientID
      let msg = ClientListChanged
      atomically $ writeTQueue roomInbox msg
      putStrLn $ "Queued messages: " <> show msg
      -- Start outbox loop.
      sendLoop conn' clientID clientOutbox
    receiver <-
      async $
        receiveLoop conn' clientID roomInbox `catch` \(e :: ConnectionException) -> case e of
          CloseRequest _ _ -> do
            putStrLn $ "Client " <> show clientID <> " disconnected"
            atomically $ do
              r@Rooms{rooms} <- readTVar tvar
              withRoom roomID (\room -> (room, room.clientOutboxes)) tvar
              let room = fromMaybe (bug $ InvariantViolated $ "Room " <> show roomID <> " does not exist") $ lookup roomID rooms
                  newRoom = room{clientOutboxes = delete clientID room.clientOutboxes}
                  newRooms = insert roomID newRoom $ delete roomID rooms
              writeTVar tvar r{rooms = newRooms}
            pass
          e' -> throwIO e'
    wait receiver
    putStrLn $ "Cleaning up after client" <> show clientID <> " disconnected"
    cancel receiver
    cancel _sender
    cancel _debug
    putStrLn $ "Cleaned up after client" <> show clientID <> " disconnected"
 where
  debugLoop :: ClientID -> IO ()
  debugLoop clientID = putStrLn ("Heartbeat: client " <> show clientID) >> delaySeconds 10 >> debugLoop clientID

  sendLoop :: Connection -> ClientID -> TQueue ServerMessage -> IO ()
  sendLoop conn' clientID clientOutbox = do
    putStrLn $ "Waiting for new message to send to client " <> show clientID
    msg <- atomically (readTQueue clientOutbox)
    putStrLn $ "Got new message to sen to client " <> show clientID <> ": " <> show msg
    sendDataMessage conn' $ WS.Text (renderHtml $ renderServerMessage msg) Nothing
    putStrLn $ "Sent message to client " <> show clientID
    sendLoop conn' clientID clientOutbox

  receiveLoop :: Connection -> ClientID -> TQueue ClientMessage -> IO ()
  receiveLoop conn' clientID roomInbox = do
    msg <- receiveDataMessage conn'
    putStrLn $ "Received message from client " <> show clientID <> ": " <> show msg
    bs <- case msg of
      WS.Text m _ -> pure m
      WS.Binary _ -> bug $ InvariantViolated $ "Received binary message from client " <> show clientID <> ": " <> show msg
    let result = eitherDecode bs >>= parseEither parseClientMessage
    clientMessage <- case result of
      Left err -> bug $ InvariantViolated $ "Failed to parse message from client " <> show clientID <> ": " <> show err
      Right cmsg -> pure cmsg
    atomically $ writeTQueue roomInbox clientMessage
    receiveLoop conn' clientID roomInbox
   where
    parseClientMessage :: Value -> Parser ClientMessage
    parseClientMessage = withObject "ClientMessage" $ \o -> do
      action :: Text <- o .: "action"
      case action of
        "set-video-id" -> do
          videoID <- o .: "video-id"
          pure $ RequestVideoID{videoID}
        _ -> parseFail $ "invalid ClientMessage action: " <> show action

nextID :: IORef Int
{-# NOINLINE nextID #-}
nextID = unsafePerformIO $ newIORef 0

generateID :: IO Text
generateID = do
  nextID' <- modifyIORef nextID (+ 1) >> readIORef nextID
  case runSqids defaultSqidsOptions{minLength = 6} $ Sqids.encode [nextID'] of
    Left err -> bug $ Impossible $ "sqid generation failed: " <> show err
    Right generated -> pure generated

data BugException = InvariantViolated String | Impossible String
  deriving stock (Show)
  deriving anyclass (Exception)

delaySeconds :: Int -> IO ()
delaySeconds = threadDelay . (* 1000000)
