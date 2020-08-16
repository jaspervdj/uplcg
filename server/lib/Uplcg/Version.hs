{-# LANGUAGE TemplateHaskell #-}
module Uplcg.Version
    ( version
    ) where

import           Control.Monad.Trans (liftIO)
import           Data.Version        (showVersion)
import qualified Language.Haskell.TH as TH
import qualified Paths_uplcg
import           System.Process      (readProcess)

version :: String
version = showVersion Paths_uplcg.version ++ " (" ++
    $(do
        hash <- liftIO $ readProcess "git" ["rev-parse", "HEAD"] ""
        pure . TH.LitE . TH.StringL $ take 8 hash) ++
    ")"
