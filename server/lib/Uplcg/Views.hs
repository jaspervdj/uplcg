{-# LANGUAGE OverloadedStrings #-}
module Uplcg.Views
    ( RoomView (..)
    , rooms
    , client
    ) where

import qualified Data.ByteString.Lazy.Builder as BLB
import           Data.Foldable                (for_)
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as T
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A
import qualified Uplcg.BaseUrl                as BaseUrl
import           Uplcg.Config
import           Uplcg.Version                (version)

data RoomView = RoomView Text Int

template :: Config -> Text -> H.Html -> H.Html
template conf title body = H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css"
            H.! A.href (H.toValue $
                BaseUrl.render (cBaseUrl conf) <> "/assets/style.css")
        H.title $ H.toHtml title
        H.meta H.! A.name "viewport" H.! A.content "width=device-width"
    H.body $ do
        body
        H.footer $ "Untitled PL Card Game version " <> H.toHtml version

rooms :: Config -> [RoomView] -> H.Html
rooms conf rids = template conf "Untitled PL Card Game" $ do
    H.h1 "Rooms"
    H.ul $ for_ rids $ \(RoomView rid num) -> H.li $ do
        H.a H.! A.href (H.toValue $
                BaseUrl.render (cBaseUrl conf) <> "/rooms/" <> rid) $
            H.toHtml rid
        " ("
        H.toHtml num
        ")"

client :: Config -> Text -> H.Html
client conf roomId = template conf "Untitled PL Card Game" $ do
    H.div H.! A.id "main" $ ""
    H.script H.! A.type_ "text/JavaScript"
        H.! A.src (H.toValue $
            BaseUrl.render (cBaseUrl conf) <> "/assets/client.js") $ ""
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
      "    '" <> t2b (BaseUrl.render $ cBaseUrl conf) <> "/rooms/" <>
          t2b roomId <> "/events';" <>
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
