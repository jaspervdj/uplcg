{-# LANGUAGE TemplateHaskell #-}
module Cafp.Messages
    ( GameView (..)
    , ServerMessage (..)
    , ClientMessage (..)
    ) where

import           Data.Text  (Text)
import           Elm.Derive

data GameView = GameView
    { gameViewOpponents  :: [Text]
    , gameViewPlayerName :: Text
    } deriving (Show)

data ServerMessage
    = Welcome Int
    | SyncGameView GameView
    | Bye
    deriving (Show)

data ClientMessage
    = ChangeName Text
    deriving (Show)

deriveBoth (defaultOptionsDropLower 8) ''GameView
deriveBoth defaultOptions ''ServerMessage
deriveBoth defaultOptions ''ClientMessage
