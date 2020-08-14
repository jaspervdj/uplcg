{-# LANGUAGE OverloadedStrings #-}
module Uplcg.Main.Server
    ( main
    ) where

import           Control.Concurrent.MVar        (MVar)
import qualified Control.Concurrent.MVar        as MVar
import           Control.Concurrent.STM         (STM, TVar, atomically)
import qualified Control.Concurrent.STM         as STM
import           Control.Exception              (bracket)
import           Control.Lens                   ((&), (.~), (^.))
import           Control.Monad                  (forever, when)
import           Control.Monad.Trans            (liftIO)
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (isAlphaNum)
import           Data.Foldable                  (for_)
import qualified Data.HashMap.Strict            as HMS
import qualified Data.List                      as L
import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.IO                   as T
import qualified Data.Text.Lazy                 as TL
import           Data.Traversable               (for)
import qualified Data.Vector                    as V
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as WS
import           System.Environment             (getEnv)
import qualified System.Log.FastLogger          as FL
import           System.Random                  (StdGen, newStdGen)
import           Text.Blaze.Html.Renderer.Text  (renderHtml)
import           Uplcg.BaseUrl                  (BaseUrl)
import qualified Uplcg.BaseUrl                  as BaseUrl
import qualified Uplcg.CookieSocket             as CookieSocket
import           Uplcg.Game
import           Uplcg.Messages
import qualified Uplcg.Views                    as Views
import qualified Web.Scotty                     as Scotty

type RoomId = T.Text

type Sink = BL.ByteString -> IO ()

data Room = Room
    { roomId    :: RoomId
    , roomGame  :: TVar Game
    , roomSinks :: TVar (HMS.HashMap PlayerId Sink)
    }

data Server = Server
    { serverBaseUrl      :: BaseUrl
    , serverLogger       :: FL.FastLogger
    , serverCookieSocket :: CookieSocket.Handle Player
    , serverCards        :: Cards
    , serverRooms        :: MVar (HMS.HashMap RoomId Room)
    }

readCards :: IO Cards
readCards = Cards
    <$> fmap parseCards (T.readFile "assets/black.txt")
    <*> fmap parseCards (T.readFile "assets/white.txt")
  where
    parseCards  = V.fromList . filter (not . T.null) . map dropComment . T.lines
    dropComment = T.strip . fst . T.break (== '#')

withServer :: BaseUrl -> FL.FastLogger -> (Server -> IO a) -> IO a
withServer base fl f = CookieSocket.withHandle 5 $ \cs -> do
    f =<< Server base fl cs <$> readCards <*> MVar.newMVar HMS.empty

newRoom :: RoomId -> Cards -> StdGen -> STM Room
newRoom rid cards gen = Room rid
    <$> STM.newTVar (newGame cards gen)
    <*> STM.newTVar HMS.empty

parseRoomId :: T.Text -> Either String T.Text
parseRoomId txt
    | T.all isAlphaNum txt && l >= 6 && l <= 32 = Right txt
    | otherwise                                 = Left "Bad room name"
  where
    l = T.length txt

roomViews :: Server -> IO [Views.RoomView]
roomViews server = do
    rooms <- liftIO . MVar.readMVar $ serverRooms server
    liftIO . for (HMS.toList rooms) $ \(rid, room) ->
        fmap (Views.RoomView rid . HMS.size) . atomically . STM.readTVar $
        roomSinks room

scottyApp :: Server -> IO Wai.Application
scottyApp server = Scotty.scottyApp $ do
    Scotty.get "/" $
        Scotty.redirect $ TL.fromStrict $
            BaseUrl.render (serverBaseUrl server) <> "/rooms"

    Scotty.get "/rooms" $ do
        views <- liftIO $ roomViews server
        Scotty.html . renderHtml $ Views.rooms (serverBaseUrl server) views

    Scotty.get "/rooms/:id/" $ do
        rid <- Scotty.param "id"
        when (T.length rid < 6) $
            Scotty.raise "Room ID should be at least 6 characters"
        Scotty.setHeader "Content-Type" "text/html"
        Scotty.file "assets/client.html"

    Scotty.get "/assets/client.js" $ do
        Scotty.setHeader "Content-Type" "application/JavaScript"
        Scotty.file "assets/client.js"

    Scotty.get "/assets/style.css" $ do
        Scotty.setHeader "Content-Type" "text/css"
        Scotty.file "assets/style.css"

routePendingConnection :: WS.PendingConnection -> Maybe RoomId
routePendingConnection pending =
    let path = T.decodeUtf8 . WS.requestPath $ WS.pendingRequest pending in
    case BaseUrl.parse path of
        BaseUrl.BaseUrl ["rooms", txt, "events"] | Right r <- parseRoomId txt ->
            Just r
        _ -> Nothing

getOrCreateRoom :: Server -> RoomId -> IO Room
getOrCreateRoom server rid = MVar.modifyMVar (serverRooms server) $ \rooms ->
    case HMS.lookup rid rooms of
        Just room -> pure (rooms, room)
        Nothing   -> do
            gen <- newStdGen
            serverLogger server $ "[" <> FL.toLogStr rid <> "] Created room"
            room <- atomically $ newRoom rid (serverCards server) gen
            pure (HMS.insert rid room rooms, room)

deleteRoom :: Server -> RoomId -> IO ()
deleteRoom server rid = do
    serverLogger server $ "[" <> FL.toLogStr rid <> "] Deleting room"
    MVar.modifyMVar_ (serverRooms server) $ pure . HMS.delete rid

joinRoom :: Room -> Sink -> Maybe Player -> STM PlayerId
joinRoom room sink mbRecovered = do
    pid <- STM.stateTVar (roomGame room) $ joinGame mbRecovered
    STM.modifyTVar' (roomSinks room) $ HMS.insert pid sink
    pure pid

leaveRoom :: Room -> PlayerId -> STM (Bool, Maybe Player)
leaveRoom room pid = do
    player <- STM.stateTVar (roomGame room) $ leaveGame pid
    STM.stateTVar (roomSinks room) $ \sinks ->
        let sinks' = HMS.delete pid sinks in
        ((HMS.null sinks', player), sinks')

syncRoom :: Server -> Room -> IO ()
syncRoom server room = do
    (game, sinks) <- atomically $ (,)
        <$> STM.stateTVar (roomGame room) (\g -> (g, g & gameLog .~ []))
        <*> STM.readTVar (roomSinks room)
    for_ (reverse $ game ^. gameLog) $ \msg ->
        serverLogger server $ "[" <> FL.toLogStr (roomId room) <> "] " <>
        FL.toLogStr msg
    for_ (HMS.toList sinks) $ \(pid, sink) -> do
        let view = gameViewForPlayer pid game
        sink . Aeson.encode $ SyncGameView view

wsApp :: Server -> WS.ServerApp
wsApp server pc = case routePendingConnection pc of
    Nothing -> WS.rejectRequest pc "Invalid URL"
    Just rid -> do
        room <- getOrCreateRoom server rid
        (conn, secret, mbRecovered) <-
            CookieSocket.acceptRequest (serverCookieSocket server) rid pc
        let sink = WS.sendTextData conn
        WS.withPingThread conn 30 (pure ()) $ bracket
            (do
                pid <- atomically $ joinRoom room sink mbRecovered
                serverLogger server $ "[" <> FL.toLogStr rid <>
                    "] Player " <> FL.toLogStr pid <>
                    if isNothing mbRecovered then " joined" else " rejoined"
                pure pid)
            (\pid -> do
                (roomEmpty, mbPlayer) <- atomically $ leaveRoom room pid
                serverLogger server $ "[" <> FL.toLogStr rid <>
                    "] Player " <> FL.toLogStr pid <> " left"
                if roomEmpty
                    then deleteRoom server rid
                    else do
                        for_ mbPlayer $ CookieSocket.persist
                            (serverCookieSocket server) secret
                        syncRoom server room)
            (\playerId -> do
                sink . Aeson.encode $ Welcome rid
                syncRoom server room
                cards <- fmap (^. gameCards) . atomically . STM.readTVar $
                    roomGame room
                sink . Aeson.encode $ SyncCards cards
                loop conn rid playerId)
  where
    loop conn rid playerId = forever $ do
        msg <- WS.receiveData conn
        case Aeson.decode msg of
            Just cm -> do
                room <- getOrCreateRoom server rid  -- TODO: only get?
                atomically . STM.modifyTVar' (roomGame room) $
                    processClientMessage playerId cm
                syncRoom server room
            Nothing -> do
                serverLogger server $ "Could not decode client message: " <>
                    FL.toLogStr (show msg)

baseUrl :: BaseUrl -> Wai.Middleware
baseUrl base@(BaseUrl.BaseUrl prefix) application = \req ->
    case L.stripPrefix prefix (Wai.pathInfo req) of
        Nothing   -> application req
        Just path -> application req
            { Wai.pathInfo = path
            , Wai.rawPathInfo = fromMaybe (Wai.rawPathInfo req) .
                B.stripPrefix bs $ Wai.rawPathInfo req
            }
  where
    bs = T.encodeUtf8 $ BaseUrl.render base

main :: IO ()
main = do
    host <- fromString <$> getEnv "UPLCG_HOSTNAME"
    port <- read <$> getEnv "UPLCG_PORT"
    base <- BaseUrl.parse . T.pack <$> getEnv "UPLCG_BASE"
    let settings = Warp.setPort port . Warp.setHost host $ Warp.defaultSettings
    timeCache <- FL.newTimeCache FL.simpleTimeFormat
    FL.withTimedFastLogger timeCache
            (FL.LogStderr FL.defaultBufSize) $ \tfl ->
        let fl s = tfl (\time -> FL.toLogStr time <> " " <> s <> "\n") in
        withServer base fl $ \server -> do
        sapp <- scottyApp server
        Warp.runSettings settings $ baseUrl base $
            WaiWs.websocketsOr WS.defaultConnectionOptions (wsApp server) sapp
