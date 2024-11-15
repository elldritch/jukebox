module Jukebox.API (app) where

import Relude
import Relude.Extra.Map (lookup)

import Network.WebSockets (PendingConnection, defaultConnectionOptions, pendingRequest)
import Servant (Capture, Get, Post, QueryParam, Raw, err404, throwError, (:<|>) (..), (:>))
import Servant.HTML.Blaze (HTML)
import Servant.Server (Application, Handler, Server, ServerError (..), err303, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Text.Blaze.Html5 (Html, a, button, em, h1, input, li, p, script, ul, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (action, class_, href, method, name, placeholder, src, type_)
import Text.Blaze.Html5.Attributes qualified as A

import Jukebox.HTML (buttonStyles, buttonStylesPrimary, classNames, frontMatter)
import Jukebox.Rooms (Room, RoomID, Rooms (..), allocateRoomAndManager)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

type Endpoints =
  Get '[HTML] Html
    :<|> "rooms" :> Post '[HTML] Html
    :<|> "rooms" :> QueryParam "id" RoomID :> Get '[HTML] Html
    :<|> "rooms" :> Capture "roomId" RoomID :> Get '[HTML] Html

type API = Endpoints :<|> "static" :> Raw

type Handler' a = ReaderT (TVar Rooms) Handler a

app :: TVar Rooms -> Application
app s = websocketsOr defaultConnectionOptions handleWebSocket $ serve (Proxy @API) (endpoints :<|> staticFiles)
 where
  readerToHandler :: TVar Rooms -> ReaderT (TVar Rooms) Handler a -> Handler a
  readerToHandler r m = runReaderT m r

  endpoints :: Server Endpoints
  endpoints =
    hoistServer
      (Proxy @Endpoints)
      (readerToHandler s)
      (indexPage :<|> createRoom :<|> joinRoom :<|> roomPage)

  handleWebSocket :: PendingConnection -> IO ()
  handleWebSocket pConn = do
    let req = pendingRequest pConn
    undefined

indexPage :: Handler' Html
indexPage = pure $ frontMatter $ do
  H.div ! class_ "mx-auto max-w-md" $ do
    h1 ! class_ "mt-8 text-2xl" $ "Speakeasy"
    H.div ! class_ "mt-4 flex items-center gap-x-2" $ do
      H.form ! action "rooms" ! method "post" $ button ! type_ "submit" ! classNames buttonStylesPrimary $ "Create room"
      "or"
      H.form ! action "rooms" ! class_ "flex grow" $ do
        H.div ! class_ "flex shrink" $ do
          H.label ! A.for "room" ! class_ "sr-only" $ "Room ID"
          input ! A.id "room" ! name "id" ! type_ "text" ! placeholder "Room ID" ! class_ "block w-28 rounded-md rounded-r-none border-0 py-1.5 px-3 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm/6"
        button ! type_ "submit" ! classNames (buttonStyles <> ["rounded-l-none"]) $ "Join room"

redirectToRoom :: RoomID -> Handler' a
redirectToRoom roomID = redirect $ "/rooms/" <> encodeUtf8 roomID

redirect :: ByteString -> Handler' a
redirect url = throwError $ err303{errHeaders = [("Location", url)]}

createRoom :: Handler' Html
createRoom = do
  tvar <- ask
  (roomID, _) <- liftIO $ allocateRoomAndManager tvar
  redirectToRoom roomID

requireRoom :: RoomID -> Handler' Room
requireRoom roomID = do
  tvar <- ask
  Rooms{rooms} <- readTVarIO tvar
  case lookup roomID rooms of
    Just room -> pure room
    Nothing ->
      throwError
        $ err404
          { errBody = renderHtml $ frontMatter $ do
              H.div ! class_ "mx-auto max-w-md" $ do
                h1 ! class_ "mt-8 text-2xl" $ ("Room " <> fromString (toString roomID) <> " does not exist")
                p ! class_ "mt-4" $ a ! class_ "text-blue-600 underline" ! href "/" $ "Back"
          }

joinRoom :: Maybe RoomID -> Handler' Html
joinRoom (Just roomID) = requireRoom roomID >> redirectToRoom roomID
joinRoom Nothing = redirect "/"

roomPage :: RoomID -> Handler' Html
roomPage roomID = do
  _ <- requireRoom roomID
  pure $ frontMatter $ do
    H.div ! class_ "mx-auto max-w-md" $ do
      h1 ! class_ "mt-8 text-2xl" $ ("Room " <> fromString (toString roomID))
    H.div ! class_ "mx-auto max-w-2xl mt-8" $ do
      H.div ! class_ "mx-auto" $ do
        H.div ! A.id "player" $ "One moment, loading player..."
    H.div ! class_ "mx-auto max-w-md mt-8" $ do
      p "Currently listening:"
      ul ! class_ "list-disc list-inside ml-4" $ do
        li $ do
          "foo "
          em "(owner)"
        li "bar"
        li "baz"
      p ! class_ "mt-4" $ a ! class_ "text-blue-600 underline" ! href "/" $ "Leave"
    script ! src "/static/room.js" $ ""
    script ! src "https://www.youtube.com/iframe_api" $ ""

staticFiles :: Server Raw
staticFiles = serveDirectoryWebApp "web"
