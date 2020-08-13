{-# LANGUAGE OverloadedStrings #-}
module Uplcg.BaseUrl
    ( BaseUrl (..)
    , parse
    , render
    ) where

import qualified Data.Text as T

newtype BaseUrl = BaseUrl [T.Text]

render :: BaseUrl -> T.Text
render (BaseUrl []) = ""
render (BaseUrl xs) = "/" <> T.intercalate "/" xs

parse :: T.Text -> BaseUrl
parse = BaseUrl . filter (not . T.null) . T.split (== '/')
