{-# LANGUAGE TemplateHaskell #-}
module Cafp.Messages
    ( ServerMessage (..)
    ) where

import           Elm.Derive

data ServerMessage
    = Welcome
    | Bye
    deriving (Show)

deriveBoth defaultOptions ''ServerMessage
