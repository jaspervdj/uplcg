{-# LANGUAGE OverloadedStrings #-}
module Cafp.Main.Server
    ( main
    ) where

import qualified Cafp.CookieSocket              as CookieSocket
import           Cafp.Game
import           Cafp.Messages
import           Control.Concurrent.MVar        (MVar)
import qualified Control.Concurrent.MVar        as MVar
import           Control.Concurrent.STM         (STM, TVar, atomically)
import qualified Control.Concurrent.STM         as STM
import           Control.Exception              (bracket)
import           Control.Lens                   ((^.))
import           Control.Monad                  (forever, when)
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
import qualified Data.Vector                    as V
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as WS
import           System.Environment             (getEnv)
import qualified System.IO                      as IO
import           System.Random                  (StdGen, newStdGen)
import qualified Web.Scotty                     as Scotty

info :: String -> IO ()
info = IO.hPutStrLn IO.stderr

type RoomId = T.Text

type Sink = BL.ByteString -> IO ()

data Room = Room
    { roomGame  :: TVar Game
    , roomSinks :: TVar (HMS.HashMap PlayerId Sink)
    }

data Server = Server
    { serverCookieSocket :: CookieSocket.Handle Player
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

withServer :: (Server -> IO a) -> IO a
withServer f = CookieSocket.withHandle 5 $ \cs ->
    f =<< Server cs <$> readCards <*> MVar.newMVar HMS.empty

newRoom :: Server -> StdGen -> STM Room
newRoom server gen = Room
    <$> (STM.newTVar $ newGame (serverCards server) gen)
    <*> STM.newTVar HMS.empty

parseRoomId :: T.Text -> Either String T.Text
parseRoomId txt
    | T.all isAlphaNum txt && T.length txt >= 6 = Right txt
    | otherwise                                 = Left "Bad room name"

scottyApp :: IO Wai.Application
scottyApp = Scotty.scottyApp $ do
    Scotty.get "/rooms/:id/" $ do
        roomId <- Scotty.param "id"
        when (T.length roomId < 6) $
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
    case splitPath path of
        ["rooms", txt, "events"] | Right r <- parseRoomId txt -> Just r
        _                                                     -> Nothing

getOrCreateRoom :: Server -> RoomId -> IO Room
getOrCreateRoom server roomId = MVar.modifyMVar (serverRooms server) $ \rooms ->
    case HMS.lookup roomId rooms of
        Just room -> pure (rooms, room)
        Nothing   -> do
            gen <- newStdGen
            info $ "[" <> T.unpack roomId <> "] Created room"
            room <- atomically $ newRoom server gen
            pure (HMS.insert roomId room rooms, room)

deleteRoom :: Server -> RoomId -> IO ()
deleteRoom server roomId = do
    info $ "[" <> T.unpack roomId <> "] Deleting room"
    MVar.modifyMVar_ (serverRooms server) $ pure . HMS.delete roomId

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

syncRoom :: Room -> IO ()
syncRoom room = do
    (game, sinks) <- atomically $ (,)
        <$> STM.readTVar (roomGame room)
        <*> STM.readTVar (roomSinks room)
    for_ (HMS.toList sinks) $ \(pid, sink) -> do
        let view = gameViewForPlayer pid game
        sink . Aeson.encode $ SyncGameView view

wsApp :: Server -> WS.ServerApp
wsApp server pc = case routePendingConnection pc of
    Nothing -> WS.rejectRequest pc "Invalid URL"
    Just roomId -> do
        room <- getOrCreateRoom server roomId
        (conn, secret, mbRecovered) <-
            CookieSocket.acceptRequest (serverCookieSocket server) roomId pc
        let sink = WS.sendTextData conn
        WS.withPingThread conn 30 (pure ()) $ bracket
            (do
                pid <- atomically $ joinRoom room sink mbRecovered
                info $ "[" <> T.unpack roomId <> "] Player " <> show pid <>
                    if isNothing mbRecovered then " joined" else " rejoined"
                pure pid)
            (\pid -> do
                (roomEmpty, mbPlayer) <- atomically $ leaveRoom room pid
                info $ "[" <> T.unpack roomId <>
                    "] Player " <> show pid <> " left"
                if roomEmpty
                    then deleteRoom server roomId
                    else do
                        for_ mbPlayer $ CookieSocket.persist
                            (serverCookieSocket server) secret
                        syncRoom room)
            (\playerId -> do
                sink . Aeson.encode $ Welcome roomId
                syncRoom room
                cards <- fmap (^. gameCards) . atomically . STM.readTVar $
                    roomGame room
                sink . Aeson.encode $ SyncCards cards
                loop conn roomId playerId)
  where
    loop conn roomId playerId = forever $ do
        msg <- WS.receiveData conn
        case Aeson.decode msg of
            Just cm -> do
                room <- getOrCreateRoom server roomId  -- TODO: only get?
                atomically . STM.modifyTVar' (roomGame room) $
                    processClientMessage playerId cm
                syncRoom room
            Nothing -> do
                info $ "Could not decode client message: " ++ show msg

splitPath :: T.Text -> [T.Text]
splitPath = filter (not . T.null) . T.split (== '/')

baseUrl :: [T.Text] -> Wai.Middleware
baseUrl prefix application = \req ->
    case L.stripPrefix prefix (Wai.pathInfo req) of
        Nothing   -> application req
        Just path -> application req
            { Wai.pathInfo = path
            , Wai.rawPathInfo = fromMaybe (Wai.rawPathInfo req) .
                B.stripPrefix bs $ Wai.rawPathInfo req
            }
  where
    bs = T.encodeUtf8 $ "/" <> T.intercalate "/" prefix

main :: IO ()
main = do
    host <- fromString <$> getEnv "CAFP_HOSTNAME"
    port <- read <$> getEnv "CAFP_PORT"
    base <- splitPath . T.pack <$> getEnv "CAFP_BASE"
    let settings = Warp.setPort port . Warp.setHost host $ Warp.defaultSettings
    withServer $ \server -> do
        sapp <- scottyApp
        Warp.runSettings settings $ baseUrl base $
            WaiWs.websocketsOr WS.defaultConnectionOptions (wsApp server) sapp
