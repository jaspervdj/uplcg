{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Uplcg.Cards
    ( Deck
    , CardSets (..)
    , loadCardSets
    ) where

import qualified Data.Aeson.TH       as Aeson
import qualified Data.HashMap.Strict as HMS
import           Data.Maybe          (fromMaybe, listToMaybe)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Yaml           as Yaml
import           Elm.Derive          (defaultOptionsDropLower)
import           Uplcg.Messages

type Deck = T.Text

data CardSets = CardSets
    { csDefault :: Maybe Deck
    , csCards   :: HMS.HashMap Deck Cards
    } deriving (Show)

data RawCardSet = RawCardSet
    { rcsDefault :: Maybe Bool
    , rcsEnabled :: Maybe Bool
    , rcsInclude :: Maybe (V.Vector T.Text)
    , rcsBlack   :: V.Vector T.Text
    , rcsWhite   :: V.Vector T.Text
    } deriving (Show)

Aeson.deriveFromJSON (defaultOptionsDropLower 3) ''RawCardSet

fromRawCardSets :: HMS.HashMap Deck RawCardSet -> CardSets
fromRawCardSets raws =
    CardSets {..}
  where
    csDefault = listToMaybe
        [ deck
        | (deck, RawCardSet {..}) <- HMS.toList raws
        , fromMaybe False rcsDefault
        ]
    csCards =
        HMS.map (\rcs ->
            let includes = V.mapMaybe (`HMS.lookup` raws) $
                    fromMaybe V.empty $ rcsInclude rcs in
            Cards
                { cardsBlack = rcsBlack rcs <> V.concatMap rcsBlack includes
                , cardsWhite = rcsWhite rcs <> V.concatMap rcsWhite includes
                }) $
        HMS.filter (fromMaybe True . rcsEnabled) raws

loadCardSets :: FilePath -> IO CardSets
loadCardSets path = fromRawCardSets <$> Yaml.decodeFileThrow path
