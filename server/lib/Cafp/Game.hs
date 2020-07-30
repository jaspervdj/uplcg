{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Cafp.Game
    ( PlayerId
    , Game (..)

    , newGame
    , joinGame
    , leaveGame

    , gameViewForPlayer
    ) where

import           Cafp.Messages
import qualified Data.HashMap.Strict as HMS
import           Data.Text           (Text)
import qualified Data.Text           as T

type PlayerId = Int

data Game = Game
    { gamePlayers      :: !(HMS.HashMap Int Text)
    , gameNextPlayerId :: !Int
    } deriving (Show)

newGame :: Game
newGame = Game HMS.empty 1

joinGame :: Game -> (PlayerId, Game)
joinGame game@Game {..} =
    let pid = gameNextPlayerId
        name = "Player " <> T.pack (show pid)
        players = HMS.insert pid name gamePlayers in
    (pid, game {gameNextPlayerId = pid + 1, gamePlayers = players})

leaveGame :: PlayerId -> Game -> Game
leaveGame pid game = game {gamePlayers = HMS.delete pid $ gamePlayers game}

gameViewForPlayer :: PlayerId -> Game -> GameView
gameViewForPlayer _ = GameView . map snd . HMS.toList . gamePlayers
