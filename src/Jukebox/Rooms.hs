module Jukebox.Rooms (
  RoomID,
  ClientID,
  Rooms (..),
  Room (..),
  allocateRoomAndManager,
) where

import Relude
import Relude.Extra.Map (insert)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, link)
import Control.Concurrent.STM (TQueue, newTQueue)
import Data.Default (Default (..))
import Network.WebSockets (DataMessage)
import Servant (FromHttpApiData)
import Text.Blaze.Html (ToMarkup)
import Web.Sqids (SqidsOptions (..), defaultSqidsOptions, encode, runSqids)

newtype RoomID = RoomID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromHttpApiData, ToString, ToMarkup)

instance ConvertUtf8 RoomID ByteString where
  encodeUtf8 = encodeUtf8 . coerce @RoomID @Text
  decodeUtf8 = coerce @Text @RoomID . decodeUtf8
  decodeUtf8Strict = (coerce @Text @RoomID <$>) . decodeUtf8Strict

newtype ClientID = ClientID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromHttpApiData)

data Rooms = Rooms
  { rooms :: Map RoomID Room
  , nextRoomID :: Int
  }

instance Default Rooms where
  def = Rooms{rooms = mempty, nextRoomID = 0}

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
  { clientOutboxes :: Map ClientID (TQueue DataMessage)
  , roomInbox :: TQueue DataMessage
  , owner :: Maybe ClientID
  , nextClientID :: Int
  }

-- Allocate a room in the TVar, and then fork a thread that manages the room.
allocateRoomAndManager :: TVar Rooms -> IO (RoomID, Async Void)
allocateRoomAndManager tvar = do
  roomID <- atomically $ do
    roomInbox <- newTQueue
    Rooms{nextRoomID, rooms} <- readTVar tvar
    let nextRoomID' = nextRoomID + 1
        roomID = generateRoomID nextRoomID'
        newRoom = Room{clientOutboxes = mempty, roomInbox, owner = Nothing, nextClientID = 0}
        rooms' = Rooms{nextRoomID = nextRoomID', rooms = insert roomID newRoom rooms}
    writeTVar tvar rooms'
    pure roomID
  roomManager <- async $ manageRoom roomID
  link roomManager
  pure (roomID, roomManager)
 where
  manageRoom roomID = do
    putStrLn $ "Managing room " <> show roomID
    threadDelay $ 10 * 1000000
    manageRoom roomID

generateRoomID :: Int -> RoomID
generateRoomID = RoomID . generateID . one

generateClientID :: Int -> Int -> ClientID
generateClientID roomID clientID = ClientID $ generateID [roomID, clientID]

-- So many of these sqids errors are programmer invariants or setup errors. I
-- don't think these errors will occur in practice (except perhaps
-- MaxEncodingAttempts). Really, I should upstream a more ergonomic API.
generateID :: [Int] -> Text
generateID ns = case runSqids defaultSqidsOptions{minLength = 6} $ encode ns of
  Left err -> bug $ Impossible $ "sqid generation failed: " <> show err
  Right x -> x

data BugException = InvariantViolated String | Impossible String
  deriving stock (Show)
  deriving anyclass (Exception)
