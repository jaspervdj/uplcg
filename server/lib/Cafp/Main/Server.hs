{-# LANGUAGE OverloadedStrings #-}
module Cafp.Main.Server
    ( main
    ) where

import           Cafp.Messages
import           Control.Concurrent.STM (STM, TVar, newTVar)
import           Control.Monad          (when)
import qualified Data.HashMap.Strict    as HMS
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Web.Scotty             as Scotty

type RoomId = T.Text

data Server = Server
    { serverRooms :: TVar (HMS.HashMap RoomId ())
    }

newServer :: STM Server
newServer = Server <$> newTVar HMS.empty

main :: IO ()
main = Scotty.scotty 3000 $ do
    Scotty.get "/rooms/:id/" $ do
        roomId <- Scotty.param "id"
        when (T.length roomId < 6) $
            Scotty.raise "Room ID should be at least 6 characters"
        Scotty.setHeader "Content-Type" "text/html"
        Scotty.file "assets/client.html"

    Scotty.get "/assets/client.js" $ do
        Scotty.setHeader "Content-Type" "application/JavaScript"
        Scotty.file "assets/client.js"
