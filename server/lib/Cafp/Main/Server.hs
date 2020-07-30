{-# LANGUAGE OverloadedStrings #-}
module Cafp.Main.Server
    ( main
    ) where

import           Cafp.Game
import           Cafp.Messages
import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.STM         (STM, TVar, atomically)
import qualified Control.Concurrent.STM         as STM
import           Control.Exception              (bracket)
import           Control.Monad                  (forever, when)
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Foldable                  (for_)
import qualified Data.HashMap.Strict            as HMS
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Lazy                 as TL
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as WS
import qualified Web.Scotty                     as Scotty

type RoomId = T.Text

type Sink = BL.ByteString -> IO ()

data Room = Room
    { roomGame    :: TVar Game
    , roomSinks   :: TVar (HMS.HashMap PlayerId Sink)
    }

data Server = Server
    { serverRooms :: TVar (HMS.HashMap RoomId Room)
    }

newServer :: STM Server
newServer = Server <$> STM.newTVar HMS.empty

newRoom :: STM Room
newRoom = Room <$> STM.newTVar newGame <*> STM.newTVar HMS.empty

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

routePendingConnection :: WS.PendingConnection -> Maybe RoomId
routePendingConnection pending =
    let path = T.decodeUtf8 . WS.requestPath $ WS.pendingRequest pending in
    case T.split (== '/') path of
        [_, "rooms", roomId, "events"] -> Just roomId
        _                              -> Nothing

getOrCreateRoom :: Server -> RoomId -> STM Room
getOrCreateRoom server roomId = do
    rooms <- STM.readTVar $ serverRooms server
    case HMS.lookup roomId rooms of
        Just room -> pure room
        Nothing   -> do
            room <- newRoom
            STM.writeTVar (serverRooms server) $ HMS.insert roomId room rooms
            pure room

joinRoom :: Room -> Sink -> STM PlayerId
joinRoom room sink = do
    pid <- STM.stateTVar (roomGame room) joinGame
    STM.modifyTVar' (roomSinks room) $ HMS.insert pid sink
    pure pid

leaveRoom :: Room -> PlayerId -> STM ()
leaveRoom room pid = do
    STM.modifyTVar' (roomGame room) $ leaveGame pid
    STM.modifyTVar' (roomSinks room) $ HMS.delete pid

syncRoom :: Room -> IO ()
syncRoom room = do
    (game, sinks) <- atomically $ (,)
        <$> STM.readTVar (roomGame room)
        <*> STM.readTVar (roomSinks room)
    for_ (HMS.toList sinks) $ \(pid, sink) ->
        sink . Aeson.encode . SyncGameView $ gameViewForPlayer pid game

wsApp :: Server -> WS.ServerApp
wsApp server pc = case routePendingConnection pc of
    Nothing -> WS.rejectRequest pc "Invalid URL"
    Just roomId -> do
        room <- atomically $ getOrCreateRoom server roomId
        conn <- WS.acceptRequest pc
        WS.withPingThread conn 30 (pure ()) $ bracket
            (atomically $ joinRoom room (WS.sendTextData conn))
            (\playerId -> do
                atomically $ leaveRoom room playerId
                syncRoom room)
            (\playerId -> do
                syncRoom room
                loop conn roomId playerId)
  where
    loop conn roomId playerId = forever $ do
        WS.sendTextData conn $ Aeson.encode Bye
        threadDelay $ 1 * 1000000

main :: IO ()
main = do
    let port = 3000
        settings = Warp.setPort port Warp.defaultSettings
    server <- atomically newServer
    sapp <- scottyApp
    Warp.runSettings settings $
        WaiWs.websocketsOr WS.defaultConnectionOptions (wsApp server) sapp
