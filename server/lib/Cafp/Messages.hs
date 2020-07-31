{-# LANGUAGE TemplateHaskell #-}
module Cafp.Messages
    ( BlackCard (..)
    , WhiteCard (..)
    , Cards (..)
    , Opponent (..)
    , TableView (..)
    , GameView (..)
    , ServerMessage (..)
    , ClientMessage (..)
    ) where

import           Data.Text   (Text)
import           Data.Vector (Vector)
import           Elm.Derive

data BlackCard = BlackCard Int deriving (Show)

data WhiteCard = WhiteCard Int deriving (Show)

data Cards = Cards
    { cardsBlack        :: Vector Text
    , cardsWhite        :: Vector Text
    } deriving (Show)

data Opponent = Opponent
    { opponentName  :: Text
    , opponentReady :: Bool
    } deriving (Show)

data TableView
    = Proposing BlackCard (Maybe WhiteCard)
    deriving (Show)

data GameView = GameView
    { gameViewOpponents :: [Opponent]
    , gameViewMyName    :: Text
    , gameViewTable     :: TableView
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
    | ProposeWhiteCards WhiteCard  -- TODO: Needs to be a list?
    deriving (Show)

deriveBoth defaultOptions ''BlackCard
deriveBoth defaultOptions ''WhiteCard
deriveBoth (defaultOptionsDropLower 5) ''Cards
deriveBoth (defaultOptionsDropLower 8) ''Opponent
deriveBoth defaultOptions ''TableView
deriveBoth (defaultOptionsDropLower 8) ''GameView
deriveBoth defaultOptions ''ServerMessage
deriveBoth defaultOptions ''ClientMessage
