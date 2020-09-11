{-# LANGUAGE OverloadedStrings #-}
module Uplcg.Views
    ( RoomView (..)
    , rooms
    , client
    ) where

import           Control.Monad                (when)
import qualified Data.ByteString.Lazy.Builder as BLB
import           Data.Foldable                (for_)
import qualified Data.HashMap.Strict          as HMS
import           Data.List                    (sort, sortBy)
import           Data.Ord                     (comparing)
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as T
import qualified Network.HTTP.Types.URI       as HttpUri
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A
import           Uplcg.Cards
import           Uplcg.Version                (version)

data RoomView = RoomView Text Bool Int

template :: Text -> H.Html -> H.Html
template title body = H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css"
            H.! A.href "/assets/style.css?v=1"
        H.title $ H.toHtml title
        H.meta H.! A.name "viewport" H.! A.content "width=device-width"
    H.body $ do
        body
        H.footer $ do
            H.a H.! A.href "https://github.com/jaspervdj/uplcg" $
                "Untitled PL Card Game"
            " version "
            H.toHtml version

rooms :: [RoomView] -> CardSets -> Maybe String -> H.Html
rooms rooms0 decks mbError = template "Untitled PL Card Game" $
    H.div H.! A.class_ "rooms" $ do
        H.h1 "Rooms"
        if null rooms0
            then H.p "No rooms online."
            else H.ul $ for_ rooms1 $ \(RoomView rid lock num) -> H.li $ do
                H.a H.! A.href (H.toValue $ "/rooms/" <> rid) $
                    H.toHtml rid
                when lock  " 🔒"
                " ("
                H.toHtml num
                ")"

        H.br
        H.h1 "Create Room"
        case mbError of
            Nothing  -> mempty
            Just err -> H.p H.! A.class_ "error" $ H.toHtml err
        H.form H.! A.method "POST" H.! A.action "/rooms" $ do
            H.label H.! A.for "name" $ "Room identifier (alphanumeric only): "
            H.input H.! A.type_ "text" H.! A.name "id"
            H.br
            H.label H.! A.for "name" $ "Password (optional): "
            H.input H.! A.type_ "text" H.! A.name "password"
            H.br
            H.label H.! A.for "deck" $ "Card set to use: "

            let sorted = sort . HMS.keys $ csCards decks
            H.select H.! A.name "deck" $ for_ sorted $ \deck ->
                if Just deck == csDefault decks then
                    H.option H.! A.value (H.toValue deck)
                        H.! A.selected "selected" $ H.toHtml deck
                else
                    H.option H.! A.value (H.toValue deck) $ H.toHtml deck

            H.br
            H.input H.! A.type_ "submit" H.! A.value "Create room"
  where
    rooms1 = sortBy (comparing (\(RoomView rid _ _) -> rid)) rooms0

client :: Text -> Maybe Text -> H.Html
client roomId mbPassword = template "Untitled PL Card Game" $ do
    H.div H.! A.id "main" $ ""
    H.script H.! A.type_ "text/JavaScript"
        H.! A.src "/assets/client.js" $ ""
    H.script H.! A.type_ "text/JavaScript" $ H.unsafeLazyByteString entryPoint
  where
    t2b = BLB.byteString . T.encodeUtf8
    entryPoint = BLB.toLazyByteString $
      "var app = Elm.Client.init({node: document.querySelector('main')});" <>

      "function connect() {" <>
      "  var protocol = 'ws:';" <>
      "  if(document.location.protocol == 'https:') {" <>
      "    protocol = 'wss:'" <>
      "  }" <>
      "  var url = protocol + '//' + document.location.host +" <>
      "    '/rooms/" <> t2b roomId <> "/events" <>
          (case mbPassword of
              Nothing -> ""
              Just pwd -> BLB.byteString $ HttpUri.renderSimpleQuery True
                  [("password", T.encodeUtf8 pwd)]) <>
              "';" <>
      "  var socket = new WebSocket(url);" <>
      "  var socketSend = function(message) {" <>
      "    socket.send(message);" <>
      "  };" <>
      "  app.ports.webSocketOut.subscribe(socketSend);" <>
      "  socket.onmessage = function(event) {" <>
      "    app.ports.webSocketIn.send(event.data);" <>
      "  };" <>
      "  socket.onclose = function(event) {" <>
      "    app.ports.webSocketOut.unsubscribe(socketSend);" <>
      "    setTimeout(function() {" <>
      "      connect();" <>
      "    }, 1000);" <>
      "  };" <>
      "  socket.onerror = function(event) {" <>
      "    socket.close();" <>
      "  };" <>
      "}" <>
      "connect();"
