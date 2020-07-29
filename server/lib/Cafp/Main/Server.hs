{-# LANGUAGE OverloadedStrings #-}
module Cafp.Main.Server
    ( main
    ) where

import           Cafp.Messages
import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.STM         (STM, TVar, newTVar)
import           Control.Monad                  (forever, when)
import qualified Data.Aeson                     as Aeson
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

data Server = Server
    { serverRooms :: TVar (HMS.HashMap RoomId ())
    }

newServer :: STM Server
newServer = Server <$> newTVar HMS.empty

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

wsApp :: WS.ServerApp
wsApp pc = case routePendingConnection pc of
    Nothing -> WS.rejectRequest pc "Invalid URL"
    Just roomId -> do
        conn <- WS.acceptRequest pc
        WS.forkPingThread conn 30
        WS.sendTextData conn $ Aeson.encode Welcome
        forever $ do
            WS.sendTextData conn $ Aeson.encode Bye
            threadDelay $ 1 * 1000000

main :: IO ()
main = do
    let port = 3000
        settings = Warp.setPort port Warp.defaultSettings
    sapp <- scottyApp
    Warp.runSettings settings $
        WaiWs.websocketsOr WS.defaultConnectionOptions wsApp sapp
