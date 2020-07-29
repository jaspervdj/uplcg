{-# LANGUAGE TemplateHaskell #-}
module Cafp.Messages
    ( ServerMessage (..)
    ) where

import           Elm.Derive

data ServerMessage
    = Welcome Int
    | Bye
    deriving (Show)

deriveBoth defaultOptions ''ServerMessage
