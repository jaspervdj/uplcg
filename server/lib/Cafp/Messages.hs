{-# LANGUAGE TemplateHaskell #-}
module Cafp.Messages
    ( GameView (..)
    , ServerMessage (..)
    ) where

import           Data.Text  (Text)
import           Elm.Derive

data GameView = GameView
    { gameViewPlayers :: [Text]
    } deriving (Show)

data ServerMessage
    = Welcome Int
    | SyncGameView GameView
    | Bye
    deriving (Show)

deriveBoth (defaultOptionsDropLower 8) ''GameView
deriveBoth defaultOptions ''ServerMessage
