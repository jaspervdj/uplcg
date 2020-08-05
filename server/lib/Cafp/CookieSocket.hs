-- | Allows websockets to reconnect and recover state by storing a cookie client
-- side.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Cafp.CookieSocket
    ( Handle
    , withHandle
    , CookieName
    , acceptRequest
    , persist
    ) where

import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar  (MVar)
import qualified Control.Concurrent.MVar  as MVar
import           Control.Monad            (forever, guard)
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HMS
import           Data.Maybe               (listToMaybe)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Time                as Time
import           Data.UUID                (UUID)
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID.V4
import qualified Network.WebSockets       as WS

type CookieName = T.Text

newtype Secret = Secret UUID deriving (Eq, Hashable)

data Handle v = Handle
    { hMinutes :: Int  -- Minutes after which expiry happens
    , hStore   :: MVar (HashMap Secret (Time.UTCTime, v))
    }

withHandle :: Int -> (Handle v -> IO a) -> IO a
withHandle minutes f = do
    store <- MVar.newMVar HMS.empty
    Async.withAsync (reaper store) $ \_ -> f $ Handle minutes store
  where
    -- This is really shitty and we should probably do something with
    -- monotonic time.
    reaper store = forever $ do
        threadDelay $ minutes * 60 * 1000 * 1000
        now <- Time.getCurrentTime
        MVar.modifyMVar_ store $ pure . HMS.filter ((> now) . fst)

parseCookie :: CookieName -> WS.PendingConnection -> Maybe T.Text
parseCookie name pc = listToMaybe $ do
    (header, values) <- WS.requestHeaders $ WS.pendingRequest pc
    guard $ header == "Cookie"
    part <- T.split (== ';') $ T.decodeUtf8 values
    let (key, val) = T.break (== '=') part
    guard $ T.strip key == name
    guard $ "=" `T.isPrefixOf` val
    pure . T.strip $ T.drop 1 val

makeCookie :: CookieName -> T.Text -> WS.Headers
makeCookie name val = [("Set-Cookie", T.encodeUtf8 $ name <> "=" <> val)]

acceptRequest
    :: Handle a -> CookieName -> WS.PendingConnection
    -> IO (WS.Connection, Secret, Maybe a)
acceptRequest h name pc = case parseCookie name pc >>= UUID.fromText of
    Just uuid -> do
        conn <- WS.acceptRequest pc
        store <- MVar.readMVar (hStore h)
        pure (conn, Secret uuid, snd <$> HMS.lookup (Secret uuid) store)
    Nothing -> do
        uuid <- UUID.V4.nextRandom
        conn <- WS.acceptRequestWith pc WS.defaultAcceptRequest
            { WS.acceptHeaders =
                makeCookie name (UUID.toText uuid) <>
                WS.acceptHeaders WS.defaultAcceptRequest
            }
        pure (conn, Secret uuid, Nothing)

persist :: Handle a -> Secret -> a -> IO ()
persist h key x = MVar.modifyMVar_ (hStore h) $ \store -> do
    expiry <- Time.addUTCTime diffTime <$> Time.getCurrentTime
    pure $ HMS.insert key (expiry, x) store
  where
    diffTime = fromIntegral (60 * hMinutes h)
