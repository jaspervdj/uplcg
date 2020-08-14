{-# LANGUAGE OverloadedStrings #-}
module Uplcg.Views
    ( RoomView (..)
    , rooms
    ) where

import           Data.Foldable               (for_)
import           Data.Text                   (Text)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Uplcg.BaseUrl               (BaseUrl)
import qualified Uplcg.BaseUrl               as BaseUrl

data RoomView = RoomView Text Int

rooms :: BaseUrl -> [RoomView] -> H.Html
rooms base rids = H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css"
            H.! A.href (H.toValue $ BaseUrl.render base <> "/assets/style.css")
    H.body $ do
        H.h1 "Rooms"
        H.ul $ for_ rids $ \(RoomView rid num) -> H.li $ do
            H.a H.! A.href (H.toValue $ BaseUrl.render base <> "/rooms/" <> rid) $
                H.toHtml rid
            " ("
            H.toHtml num
            ")"
        H.footer $ "Untitled PL Card Game"
