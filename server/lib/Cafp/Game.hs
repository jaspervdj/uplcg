{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
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
import           Control.Lens              (Lens', at, ix, over, to, traverseOf,
                                            (%%=), (%=), (%~), (&), (.~), (^.),
                                            (^..), (^?), _1, _2, (.=))
import           Control.Lens.TH           (makeLenses, makePrisms)
import           Control.Monad             (guard, replicateM, (>=>))
import           Control.Monad.State       (State, state, execState, runState)
import qualified Data.HashMap.Strict       as HMS
import           Data.Maybe                (fromMaybe)
import Data.Bifunctor (first)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           System.Random             (StdGen)
import           VectorShuffling.Immutable (shuffle)

type PlayerId = Int

data Table
    = TableProposing BlackCard (HMS.HashMap PlayerId [WhiteCard])
    deriving (Show)

data Player = Player
    { _playerName :: Text
    , _playerHand :: [WhiteCard]
    } deriving (Show)

data Game = Game
    { _gameCards        :: !Cards
    , _gameSeed         :: !StdGen
    , _gameBlack        :: ![BlackCard]
    , _gameWhite        :: ![WhiteCard]
    , _gamePlayers      :: !(HMS.HashMap PlayerId Player)
    , _gameTable        :: !Table
    , _gameNextPlayerId :: !Int
    } deriving (Show)

makePrisms ''Table
makeLenses ''Player
makeLenses ''Game

popCard
    :: (Cards -> V.Vector t) -> (Int -> c) -> Lens' Game [c]
    -> State Game c
popCard getDeck mk queue = state $ \game -> case game ^. queue of
    (x : xs) -> (x, game & queue .~ xs)
    []        ->
        let deck = game ^. gameCards . to getDeck
            idxs = V.imap (\i _ -> mk i) deck
            (cs, seed) = first V.toList $ shuffle idxs (game ^. gameSeed) in
        case cs of
            []     -> error "popCard: Cards are empty"
            x : xs -> (x, game & queue .~ xs & gameSeed .~ seed)

popBlackCard :: State Game BlackCard
popBlackCard = popCard cardsBlack BlackCard gameBlack

popWhiteCard :: State Game WhiteCard
popWhiteCard = popCard cardsWhite WhiteCard gameWhite

newGame :: Cards -> StdGen -> Game
newGame cards gen = flip execState state0 $ do
    black <- popBlackCard
    gameTable .= TableProposing black HMS.empty
  where
    state0 = Game
        { _gameCards        = cards
        , _gameSeed         = gen
        , _gameBlack        = []
        , _gameWhite        = []
        , _gamePlayers      = HMS.empty
        , _gameTable        = TableProposing (BlackCard 0) HMS.empty
        , _gameNextPlayerId = 1
        }

joinGame :: Game -> (PlayerId, Game)
joinGame = runState $ do
    pid <- gameNextPlayerId %%= (\x -> (x, x + 1))
    let name = "Player " <> T.pack (show pid)
    hand <- replicateM 6 popWhiteCard
    gamePlayers %= HMS.insert pid (Player name hand)
    pure pid

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
