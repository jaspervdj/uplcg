module Cafp.InfiniteDeck
    ( InfiniteDeck
    , new
    , newIO
    , pop
    , popN
    ) where

import           Data.List                 (intercalate)
import qualified Data.Vector               as V
import           System.Random             (StdGen, newStdGen)
import           VectorShuffling.Immutable (shuffle)

newtype InfiniteDeck a = InfiniteDeck [a]

instance Show a => Show (InfiniteDeck a) where
    show (InfiniteDeck xs) =
        "[" ++ intercalate ", " (map show $ take 5 xs) ++ "...]"

new :: V.Vector a -> StdGen -> InfiniteDeck a
new vec gen0
    | V.null vec = error "Cafp.InfiniteDeck.new: empty vector"
    | otherwise  = InfiniteDeck (V.toList x ++ xs)
  where
    (x, gen1) = shuffle vec gen0
    InfiniteDeck xs = new vec gen1

newIO :: V.Vector a -> IO (InfiniteDeck a)
newIO vec = new vec <$> newStdGen

pop :: InfiniteDeck a -> (a, InfiniteDeck a)
pop (InfiniteDeck [])       = error "Cafp.InfiniteDeck.pop: empty"
pop (InfiniteDeck (x : xs)) = (x, InfiniteDeck xs)

popN :: Int -> InfiniteDeck a -> ([a], InfiniteDeck a)
popN n (InfiniteDeck xs) = let (ys, zs) = splitAt n xs in (ys, InfiniteDeck zs)
