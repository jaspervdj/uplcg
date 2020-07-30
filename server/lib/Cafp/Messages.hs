{-# LANGUAGE TemplateHaskell #-}
module Cafp.Messages
    ( BlackCard (..)
    , WhiteCard (..)
    , GameView (..)
    , ServerMessage (..)
    , ClientMessage (..)
    ) where

import           Data.Text  (Text)
import           Elm.Derive

data BlackCard = BlackCard Text deriving (Show)

data WhiteCard = WhiteCard Text deriving (Show)

data GameView = GameView
    { gameViewOpponents :: [Text]
    , gameViewMyName    :: Text
    , gameViewBlackCard :: Maybe BlackCard
    , gameViewHand      :: [WhiteCard]
    } deriving (Show)

data ServerMessage
    = Welcome Int
    | SyncGameView GameView
    | Bye
    deriving (Show)

data ClientMessage
    = ChangeMyName Text
    deriving (Show)

deriveBoth defaultOptions ''BlackCard
deriveBoth defaultOptions ''WhiteCard
deriveBoth (defaultOptionsDropLower 8) ''GameView
deriveBoth defaultOptions ''ServerMessage
deriveBoth defaultOptions ''ClientMessage
