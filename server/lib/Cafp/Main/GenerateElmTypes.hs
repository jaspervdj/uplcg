{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Cafp.Main.GenerateElmTypes
    ( main
    ) where

import           Cafp.Messages
import           Data.Proxy
import           Elm.Module

main :: IO ()
main = putStrLn $ makeElmModule "Messages"
    [ DefineElm (Proxy :: Proxy ServerMessage)
    ]
