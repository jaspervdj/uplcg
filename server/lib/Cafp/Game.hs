{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE Rank2Types #-}
module Cafp.Game
    ( PlayerId
    , Table (..)
    , Player (..)
    , Game (..)
    , gameCards, gamePlayers, gameNextPlayerId

    , newGame
    , joinGame
    , leaveGame

    , processClientMessage

    , gameViewForPlayer
    ) where

import           Cafp.Messages
import           Control.Lens              (at, ix, over, to, (%~), (&), (.~),
                                            (^.), (^..), (^?), _1, _2, traverseOf, Lens')
import           Control.Lens.TH           (makeLenses, makePrisms)
import           Control.Monad             (guard, (>=>))
import qualified Data.HashMap.Strict       as HMS
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Cafp.InfiniteDeck as InfiniteDeck
import Cafp.InfiniteDeck (InfiniteDeck)
import qualified Data.Vector               as V

type PlayerId = Int

data Table
    = TableProposing BlackCard (HMS.HashMap PlayerId [WhiteCard])
    deriving (Show)

data Player = Player
    { _playerName       :: Text
    , _playerHand       :: [WhiteCard]
    } deriving (Show)

data Game = Game
    { _gameCards        :: !Cards
    , _gameBlack        :: !(InfiniteDeck BlackCard)
    , _gameWhite        :: !(InfiniteDeck WhiteCard)
    , _gamePlayers      :: !(HMS.HashMap PlayerId Player)
    , _gameTable        :: !Table
    , _gameNextPlayerId :: !Int
    } deriving (Show)

makePrisms ''Table
makeLenses ''Player
makeLenses ''Game

newGame :: Cards -> IO Game
newGame cards = do
    black <- fmap InfiniteDeck.pop . newDeck BlackCard $ cardsBlack cards
    white <- newDeck WhiteCard $ cardsWhite cards
    pure Game
        { _gameCards        = cards
        , _gameBlack        = snd black
        , _gameWhite        = white
        , _gamePlayers      = HMS.empty
        , _gameTable        = TableProposing (fst black) HMS.empty
        , _gameNextPlayerId = 1
        }
  where
    newDeck f = InfiniteDeck.newIO . V.imap (\i _ -> f i)

joinGame :: Game -> (PlayerId, Game)
joinGame game =
    let pid = game ^. gameNextPlayerId
        name = "Player " <> T.pack (show pid)
        (hand, white) = InfiniteDeck.popN 6 (game ^. gameWhite) in
    ( pid
    , game
        & gameNextPlayerId %~ succ
        & gamePlayers %~ HMS.insert pid (Player name hand)
        & gameWhite .~ white
    )

leaveGame :: PlayerId -> Game -> Game
leaveGame pid = over gamePlayers $ HMS.delete pid

blackCardBlanks :: Cards -> BlackCard -> Int
blackCardBlanks cards (BlackCard c) =
    maybe 0 (length . T.breakOnAll "\\BLANK") $ cardsBlack cards V.!? c

processClientMessage :: PlayerId -> ClientMessage -> Game -> Game
processClientMessage pid msg game = case msg of
    ChangeMyName name ->
        game & gamePlayers . ix pid . playerName .~ name
    ProposeWhiteCards cs
        -- Bad card(s) proposed.
        | any (not . (`elem` hand)) cs -> game
        -- Proposal already made.
        | Just _ <- game ^? gameTable . _TableProposing . _2 . ix pid -> game
        -- Not enough cards submitted.
        | Just b <- game ^? gameTable . _TableProposing . _1
        , blackCardBlanks (game ^. gameCards) b /= length cs -> game
        -- TODO: Check that the card is in the hand of the player.
        | otherwise                                  ->
            game & gameTable . _TableProposing . _2 . at pid .~ Just cs
  where
    hand = game ^.. gamePlayers . ix pid . playerHand . traverse

gameViewForPlayer :: PlayerId -> Game -> GameView
gameViewForPlayer self game =
    let opponents = do
            (pid, p) <- HMS.toList $ game ^. gamePlayers
            guard $ pid /= self
            pure $ Opponent (p ^. playerName) $ case game ^. gameTable of
                TableProposing _ proposals -> HMS.member pid proposals

        player = game ^. gamePlayers . at self

        table = case game ^. gameTable of
            TableProposing black proposals ->
                Proposing black . fromMaybe [] $ HMS.lookup self proposals in
    GameView
        { gameViewOpponents = opponents
        , gameViewMyName    = maybe "" (^. playerName) player
        , gameViewTable     = table
        , gameViewHand      = maybe [] (^. playerHand) player
        }
