{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Uplcg.Main.GenerateElmTypes
    ( main
    ) where

import           Uplcg.Messages
import           Data.Proxy
import           Elm.Module

main :: IO ()
main = putStrLn $ makeElmModule "Messages"
    [ DefineElm (Proxy :: Proxy BlackCard)
    , DefineElm (Proxy :: Proxy WhiteCard)
    , DefineElm (Proxy :: Proxy Cards)
    , DefineElm (Proxy :: Proxy PlayerView)
    , DefineElm (Proxy :: Proxy VotedView)
    , DefineElm (Proxy :: Proxy TableView)
    , DefineElm (Proxy :: Proxy GameView)
    , DefineElm (Proxy :: Proxy ServerMessage)
    , DefineElm (Proxy :: Proxy ClientMessage)
    ]
