module Jukebox.API (app) where

import Relude

import Data.ByteString.Char8 qualified as BSC
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (
  PendingConnection,
  RejectRequest (..),
  RequestHead (..),
  acceptRequest,
  defaultConnectionOptions,
  defaultRejectRequest,
  pendingRequest,
  rejectRequestWith,
 )
import Network.WebSockets.Connection (pendingStream)
import Network.WebSockets.Stream (close)
import Servant (Capture, Get, Post, QueryParam, Raw, err404, throwError, (:<|>) (..), (:>))
import Servant.HTML.Blaze (HTML)
import Servant.Server (Application, Handler, Server, ServerError (..), err303, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html, a, button, form, h1, input, label, noscript, p, toHtml, ul, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (action, class_, href, method, name, placeholder, type_, value)
import Text.Blaze.Html5.Attributes qualified as A

import Jukebox.HTML (buttonStyles, buttonStylesPrimary, classNames, frontMatter, hx, ws)
import Jukebox.Rooms (RoomID, Rooms, lookupRoom, startClient, startRoom)

type Endpoints =
  Get '[HTML] Html
    :<|> "rooms" :> Post '[HTML] Html
    :<|> "rooms" :> QueryParam "id" Text :> Get '[HTML] Html
    :<|> "rooms" :> Capture "roomId" Text :> Get '[HTML] Html

type API = Endpoints :<|> "static" :> Raw

type Handler' a = ReaderT (TVar Rooms) Handler a

app :: TVar Rooms -> Application
app tvar = websocketsOr defaultConnectionOptions handleWebSocket $ serve (Proxy @API) (endpoints :<|> staticFiles)
 where
  endpoints :: Server Endpoints
  endpoints =
    hoistServer
      (Proxy @Endpoints)
      (`runReaderT` tvar)
      (indexPage :<|> createRoom :<|> joinRoom :<|> roomPage)

  handleWebSocket :: PendingConnection -> IO ()
  handleWebSocket pConn = do
    let req = pendingRequest pConn
    putStrLn $ "Received websocket connection: " <> show req
    case BSC.split '/' req.requestPath of
      ["", "rooms", rid] -> do
        putStrLn $ "Connecting to room " <> show rid
        rooms <- readTVarIO tvar
        case lookupRoom (decodeUtf8 rid) rooms of
          Just (roomID, _) -> do
            conn <- acceptRequest pConn
            putStrLn "Accepted websocket connection"
            startClient roomID tvar conn
            putStrLn "Websocket connection closed"
          Nothing -> do
            putStrLn "Room does not exist"
            rejectAndClose defaultRejectRequest{rejectBody = "room does not exist", rejectCode = 404}
      path -> do
        putStrLn $ "Invalid websocket path: " <> show path
        rejectAndClose defaultRejectRequest{rejectBody = "invalid request path"}
   where
    rejectAndClose req = do
      putStrLn "Rejecting and closing websocket connection"
      rejectRequestWith pConn req
      close $ pendingStream pConn
      putStrLn "Websocket rejected and closed"

indexPage :: Handler' Html
indexPage = pure $ frontMatter $ do
  H.div ! class_ "mx-auto max-w-md" $ do
    h1 ! class_ "mt-8 text-2xl" $ "Speakeasy"
    H.div ! class_ "mt-4 flex items-center gap-x-2" $ do
      form ! action "rooms" ! method "post" $ button ! type_ "submit" ! classNames buttonStylesPrimary $ "Create room"
      "or"
      form ! action "rooms" ! class_ "flex grow" $ do
        H.div ! class_ "flex shrink" $ do
          label ! A.for "room" ! class_ "sr-only" $ "Room ID"
          input ! A.id "room" ! name "id" ! type_ "text" ! placeholder "Room ID" ! class_ "block w-28 rounded-md rounded-r-none border-0 py-1.5 px-3 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm/6"
        button ! type_ "submit" ! classNames (buttonStyles <> ["rounded-l-none"]) $ "Join room"

redirect :: ByteString -> Handler' a
redirect url = throwError $ err303{errHeaders = [("Location", url)]}

redirectToRoom :: RoomID -> Handler' a
redirectToRoom roomID = redirect $ "/rooms/" <> encodeUtf8 (toText roomID)

createRoom :: Handler' Html
createRoom = ask >>= liftIO . startRoom >>= redirectToRoom

requireRoom :: Text -> Handler' RoomID
requireRoom rid = do
  rooms <- ask >>= readTVarIO
  case lookupRoom rid rooms of
    Just (roomID, _) -> pure roomID
    Nothing -> throwError $ err404{errBody = roomNotFound}
 where
  roomNotFound = renderHtml $ frontMatter $ do
    H.div ! class_ "mx-auto max-w-md" $ do
      h1 ! class_ "mt-8 text-2xl" $ toHtml $ "Room " <> toText rid <> " does not exist"
      p ! class_ "mt-4" $ a ! class_ "text-blue-600 underline" ! href "/" $ "Back"

joinRoom :: Maybe Text -> Handler' Html
joinRoom (Just rid) = requireRoom rid >>= redirectToRoom
joinRoom Nothing = redirect "/"

roomPage :: Text -> Handler' Html
roomPage rid = do
  roomID <- requireRoom rid
  pure $ frontMatter $ do
    H.div ! hx "ext" "ws" ! ws "connect" ("/rooms/" <> toText roomID) $ do
      H.div ! class_ "mx-auto max-w-md" $ h1 ! class_ "mt-8 text-2xl" $ toHtml $ "Room " <> toText roomID
      H.div ! class_ "mx-auto max-w-2xl mt-8" $ H.div ! class_ "mx-auto" $ H.div ! A.id "player" $ pass
      H.div ! class_ "mx-auto max-w-md mt-8" $ do
        form ! class_ "flex" ! ws "send" "" $ do
          H.div $ do
            label ! A.for "video-id" ! class_ "sr-only" $ "YouTube Video ID"
            input ! A.id "video-id" ! name "video-id" ! type_ "text" ! placeholder "Video ID" ! class_ "block max-w-full rounded-md rounded-r-none border-0 py-1.5 px-3 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm/6"
            input ! type_ "hidden" ! name "action" ! value "set-video-id"
          button ! type_ "submit" ! classNames (buttonStyles <> ["rounded-l-none"]) $ "Set Video"
        p ! class_ "mt-4" $ "Currently listening:"
        noscript "Sorry, this app requires JavaScript to function."
        ul ! class_ "list-disc list-inside ml-4" ! A.id "listeners" $ pass
        p ! class_ "mt-4" $ a ! class_ "text-blue-600 underline" ! href "/" $ "Leave"
      H.div ! A.id "player-state" $ pass

staticFiles :: Server Raw
staticFiles = serveDirectoryWebApp "web"
