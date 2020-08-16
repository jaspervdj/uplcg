module Uplcg.Config
    ( Config (..)
    , fromEnv
    ) where

import qualified Data.Text          as T
import           System.Environment (getEnv)
import           Uplcg.BaseUrl      (BaseUrl)
import qualified Uplcg.BaseUrl      as BaseUrl

data Config = Config
    { cHostname :: String
    , cPort     :: Int
    , cBaseUrl  :: BaseUrl
    } deriving (Show)

fromEnv :: IO Config
fromEnv = Config
    <$> getEnv "UPLCG_HOSTNAME"
    <*> (read <$> getEnv "UPLCG_PORT")
    <*> (BaseUrl.parse . T.pack <$> getEnv "UPLCG_BASE")
