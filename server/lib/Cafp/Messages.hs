{-# LANGUAGE DeriveGeneric   #-}
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

import           Data.Hashable (Hashable)
import           Data.Text     (Text)
import           Data.Vector   (Vector)
import           Elm.Derive
import           GHC.Generics  (Generic)

data BlackCard = BlackCard Int deriving (Eq, Generic, Show)

instance Hashable BlackCard

data WhiteCard = WhiteCard Int deriving (Eq, Generic, Show)

instance Hashable WhiteCard

data Cards = Cards
    { cardsBlack :: Vector Text
    , cardsWhite :: Vector Text
    } deriving (Show)

data Opponent = Opponent
    { opponentName  :: Text
    , opponentReady :: Bool
    } deriving (Show)

data TableView
    = Proposing BlackCard [WhiteCard]
    | Voting
        BlackCard
        [[WhiteCard]]  -- ^ Proposals to vote for
        Int            -- ^ My proposal
        (Maybe Int)    -- ^ My vote
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
    | ProposeWhiteCards [WhiteCard]
    | SubmitVote Int
    deriving (Show)

deriveBoth defaultOptions ''BlackCard
deriveBoth defaultOptions ''WhiteCard
deriveBoth (defaultOptionsDropLower 5) ''Cards
deriveBoth (defaultOptionsDropLower 8) ''Opponent
deriveBoth defaultOptions ''TableView
deriveBoth (defaultOptionsDropLower 8) ''GameView
deriveBoth defaultOptions ''ServerMessage
deriveBoth defaultOptions ''ClientMessage
