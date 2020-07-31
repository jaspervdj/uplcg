{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Cafp.Game
    ( PlayerId
    , Cards (..)
    , Game (..)
    , gameCards, gamePlayers, gameNextPlayerId

    , newGame
    , joinGame
    , leaveGame

    , processClientMessage

    , gameViewForPlayer
    ) where

import           Cafp.Messages
import           Control.Lens        (at, ix, over, to, (%~), (&), (.~), (^.),
                                      (^?), _2)
import           Control.Lens.TH     (makeLenses, makePrisms)
import qualified Data.HashMap.Strict as HMS
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V

type PlayerId = Int

data Table
    = TableProposing BlackCard (HMS.HashMap PlayerId WhiteCard)
    deriving (Show)

data Game = Game
    { _gameCards        :: !Cards
    , _gamePlayers      :: !(HMS.HashMap PlayerId Text)
    , _gameTable        :: !Table
    , _gameNextPlayerId :: !Int
    } deriving (Show)

makePrisms ''Table
makeLenses ''Game

newGame :: Cards -> Game
newGame cards = Game
    { _gameCards        = cards
    , _gamePlayers      = HMS.empty
    , _gameTable        = TableProposing (BlackCard 0) HMS.empty
    , _gameNextPlayerId = 1
    }

joinGame :: Game -> (PlayerId, Game)
joinGame game =
    let pid = game ^. gameNextPlayerId
        name = "Player " <> T.pack (show pid) in
    ( pid
    , game & gameNextPlayerId %~ succ & gamePlayers %~ HMS.insert pid name
    )

leaveGame :: PlayerId -> Game -> Game
leaveGame pid = over gamePlayers $ HMS.delete pid

validWhiteCard :: Cards -> WhiteCard -> Bool
validWhiteCard cards (WhiteCard c) =
    let len = V.length $ cardsWhite cards in c >= 0 && c < len

processClientMessage :: PlayerId -> ClientMessage -> Game -> Game
processClientMessage pid msg game = case msg of
    ChangeMyName name ->
        game & gamePlayers . ix pid .~ name
    ProposeWhiteCards c
        -- Bad card proposed.
        | not $ validWhiteCard (game ^. gameCards) c -> game
        -- Proposal already made.
        | Just _ <- game ^? gameTable . _TableProposing . _2 . at pid -> game
        -- TODO: Check that the card is in the hand of the player.
        | otherwise                                  ->
            game & gameTable . _TableProposing . _2 . at pid .~ Just c

gameViewForPlayer :: PlayerId -> Game -> GameView
gameViewForPlayer self game =
    let opponents = map snd . HMS.toList . HMS.delete self $ game ^. gamePlayers
        name = fromMaybe "" $ game ^. gamePlayers . at self

        table = case game ^. gameTable of
            TableProposing black proposals ->
                Proposing black (HMS.lookup self proposals) in
    GameView
        { gameViewOpponents = opponents
        , gameViewMyName    = name
        , gameViewTable     = table
        , gameViewHand      = [WhiteCard x | x <- [0 .. 9]]
        }
