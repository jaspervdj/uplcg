module Uplcg.Cards
    ( Deck
    , CardSets
    , loadCardSets
    ) where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text           as T
import qualified Data.Yaml           as Yaml
import           Uplcg.Messages

type Deck = T.Text

type CardSets = HMS.HashMap Deck Cards

loadCardSets :: FilePath -> IO CardSets
loadCardSets path = Yaml.decodeFileThrow path
