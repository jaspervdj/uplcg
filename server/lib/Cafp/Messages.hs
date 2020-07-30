{-# LANGUAGE TemplateHaskell #-}
module Cafp.Messages
    ( BlackCard (..)
    , WhiteCard (..)
    , Cards (..)
    , GameView (..)
    , ServerMessage (..)
    , ClientMessage (..)
    ) where

import           Data.Text  (Text)
import           Elm.Derive

data BlackCard = BlackCard Int deriving (Show)

data WhiteCard = WhiteCard Int deriving (Show)

data Cards = Cards
    { cardsBlack :: [Text]
    , cardsWhite :: [Text]
    } deriving (Show)

data GameView = GameView
    { gameViewOpponents :: [Text]
    , gameViewMyName    :: Text
    , gameViewBlackCard :: Maybe BlackCard
    , gameViewHand      :: [WhiteCard]
    } deriving (Show)

data ServerMessage
    = Welcome Int
    | SyncCards Cards
    | SyncGameView GameView
    | Bye
    deriving (Show)

data ClientMessage
    = ChangeMyName Text
    deriving (Show)

deriveBoth defaultOptions ''BlackCard
deriveBoth defaultOptions ''WhiteCard
deriveBoth (defaultOptionsDropLower 5) ''Cards
deriveBoth (defaultOptionsDropLower 8) ''GameView
deriveBoth defaultOptions ''ServerMessage
deriveBoth defaultOptions ''ClientMessage
