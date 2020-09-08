{-# LANGUAGE TemplateHaskell #-}
module Uplcg.Cards
    ( Deck
    , CardSets
    , loadCardSets
    ) where

import qualified Data.Aeson.TH       as Aeson
import qualified Data.HashMap.Strict as HMS
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Yaml           as Yaml
import           Elm.Derive          (defaultOptionsDropLower)
import           Uplcg.Messages

type Deck = T.Text

type CardSets = HMS.HashMap Deck Cards

data RawCardSet = RawCardSet
    { rcsEnabled :: Maybe Bool
    , rcsInclude :: Maybe (V.Vector T.Text)
    , rcsBlack   :: V.Vector T.Text
    , rcsWhite   :: V.Vector T.Text
    }

Aeson.deriveFromJSON (defaultOptionsDropLower 3) ''RawCardSet

fromRawCardSets :: HMS.HashMap Deck RawCardSet -> CardSets
fromRawCardSets raws =
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
