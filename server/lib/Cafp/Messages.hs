{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Cafp.Messages
    ( BlackCard (..)
    , WhiteCard (..)
    , Cards (..)
    , PlayerView (..)
    , VotedView (..)
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
    { cardsBlack :: !(Vector Text)
    , cardsWhite :: !(Vector Text)
    } deriving (Show)

data PlayerView = PlayerView
    { playerViewName   :: !Text
    , playerViewAdmin  :: !Bool
    , playerViewReady  :: !Bool
    , playerViewPoints :: !Int
    } deriving (Show)

data VotedView = VotedView
    { votedProposal :: !(Vector WhiteCard)
    , votedScore    :: !Int
    , votedWinners  :: !(Vector Text)
    } deriving (Show)

data TableView
    = Proposing !BlackCard !(Vector WhiteCard)
    | Voting
        !BlackCard
        !(Vector (Vector WhiteCard))  -- ^ Proposals to vote for
        !(Maybe Int)                  -- ^ My proposal
        !(Maybe Int)                  -- ^ My vote
    | Tally !BlackCard !(Vector VotedView)
    deriving (Show)

data GameView = GameView
    { gameViewPlayers :: !(Vector PlayerView)
    , gameViewMe      :: !PlayerView
    , gameViewTable   :: !TableView
    , gameViewHand    :: !(Vector WhiteCard)
    } deriving (Show)

data ServerMessage
    = Welcome !Text
    | SyncCards !Cards
    | SyncGameView !GameView
    deriving (Show)

data ClientMessage
    = ChangeMyName !Text
    | ProposeWhiteCards !(Vector WhiteCard)
    | SubmitVote !Int
    | AdminSkipProposals
    | AdminSkipVotes
    | AdminConfirmTally
    deriving (Show)

deriveBoth defaultOptions ''BlackCard
deriveBoth defaultOptions ''WhiteCard
deriveBoth (defaultOptionsDropLower 5) ''Cards
deriveBoth (defaultOptionsDropLower 10) ''PlayerView
deriveBoth (defaultOptionsDropLower 5) ''VotedView
deriveBoth defaultOptions ''TableView
deriveBoth (defaultOptionsDropLower 8) ''GameView
deriveBoth defaultOptions ''ServerMessage
deriveBoth defaultOptions ''ClientMessage
