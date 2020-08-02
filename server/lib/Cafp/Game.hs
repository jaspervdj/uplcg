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
                                            (^..), (^?), _1, _2, (.=), _3)
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

type Proposal = [WhiteCard]

data Table
    = TableProposing
        !BlackCard
        !(HMS.HashMap PlayerId Proposal)
    | TableVoting
        !BlackCard
        !(V.Vector (Proposal, [PlayerId]))
        !(HMS.HashMap PlayerId Int)
    deriving (Show)

data Player = Player
    { _playerName :: !Text
    , _playerHand :: !(V.Vector WhiteCard)
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
    hand <- V.replicateM 6 popWhiteCard
    gamePlayers %= HMS.insert pid (Player name hand)
    pure pid

leaveGame :: PlayerId -> Game -> Game
leaveGame pid = over gamePlayers $ HMS.delete pid

blackCardBlanks :: Cards -> BlackCard -> Int
blackCardBlanks cards (BlackCard c) =
    maybe 0 (length . T.breakOnAll "\\BLANK") $ cardsBlack cards V.!? c

stepGame :: Game -> Game
stepGame game = case game ^. gameTable of
    TableProposing black proposals
        | HMS.null ((game ^. gamePlayers) `HMS.difference` proposals) ->
            let proposalsMap = HMS.fromListWith (++) $ do
                    (pid, proposal) <- HMS.toList proposals
                    pure (proposal, [pid])
                (shuffled, seed) = shuffle
                    (V.fromList $ HMS.toList proposalsMap) (game ^. gameSeed) in
            game & gameSeed .~ seed
                & gameTable .~ TableVoting black shuffled HMS.empty
        | otherwise -> game
    TableVoting _ _ _ -> game

processClientMessage :: PlayerId -> ClientMessage -> Game -> Game
processClientMessage pid msg game = case msg of
    ChangeMyName name ->
        game & gamePlayers . ix pid . playerName .~ name
    ProposeWhiteCards cs
        -- Bad card(s) proposed, i.e. not in hand of player.
        | any (not . (`elem` hand)) cs -> game
        -- Proposal already made.
        | Just _ <- game ^? gameTable . _TableProposing . _2 . ix pid -> game
        -- Not enough cards submitted.
        | Just b <- game ^? gameTable . _TableProposing . _1
        , blackCardBlanks (game ^. gameCards) b /= length cs -> game
        -- All good.
        | otherwise -> stepGame $
            game & gameTable . _TableProposing . _2 . at pid .~ Just cs

    SubmitVote i -> case game ^. gameTable of
        TableProposing _ _ -> game
        TableVoting _ shuffled votes
            -- Vote out of bounds.
            | i < 0 || i >= V.length shuffled -> game
            -- Already voted.
            | pid `HMS.member` votes -> game
            -- Can't vote for self.
            | pid `elem` snd (shuffled V.! i) -> game
            -- Ok vote.
            | otherwise -> stepGame $ game
                & gameTable . _TableVoting . _3 . at pid .~ Just i
  where
    hand = game ^.. gamePlayers . ix pid . playerHand . traverse

gameViewForPlayer :: PlayerId -> Game -> GameView
gameViewForPlayer self game =
    let opponents = do
            (pid, p) <- HMS.toList $ game ^. gamePlayers
            guard $ pid /= self
            pure $ Opponent (p ^. playerName) $ case game ^. gameTable of
                TableProposing _ proposals -> HMS.member pid proposals
                TableVoting _ _ votes -> HMS.member pid votes

        player = game ^. gamePlayers . at self

        table = case game ^. gameTable of
            TableProposing black proposals ->
                Proposing black . fromMaybe [] $ HMS.lookup self proposals
            TableVoting black shuffled votes -> Voting
                black
                (fst <$> V.toList shuffled)
                (fromMaybe 0 $ V.findIndex ((self `elem`) . snd) shuffled)
                (HMS.lookup self votes) in
    GameView
        { gameViewOpponents = opponents
        , gameViewMyName    = maybe "" (^. playerName) player
        , gameViewTable     = table
        , gameViewHand      = player ^.. traverse . playerHand . traverse
        }
