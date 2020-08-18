{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Uplcg.Main.Server
    ( main
    ) where

import           Control.Concurrent.MVar         (MVar)
import qualified Control.Concurrent.MVar         as MVar
import           Control.Concurrent.STM          (STM, TVar, atomically)
import qualified Control.Concurrent.STM          as STM
import           Control.Exception               (bracket)
import           Control.Lens                    ((&), (.~), (^.))
import           Control.Monad                   (forever)
import           Control.Monad.Trans             (liftIO)
import qualified Data.Aeson                      as Aeson
import           Data.Bifunctor                  (first, second)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as BC
import qualified Data.ByteString.Lazy            as BL
import           Data.Char                       (isAlphaNum)
import           Data.Foldable                   (for_)
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HMS
import           Data.Maybe                      (isNothing)
import           Data.String                     (fromString)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as TL
import           Data.Traversable                (for)
import qualified Data.Vector                     as V
import qualified Network.HTTP.Types.Status       as HttpStatus
import qualified Network.HTTP.Types.URI          as HttpUri
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Handler.Warp        as Warp
import qualified Network.Wai.Handler.WebSockets  as WaiWs
import qualified Network.Wai.Middleware.HttpAuth as HttpAuth
import qualified Network.WebSockets              as WS
import           System.Environment              (getEnv)
import qualified System.Log.FastLogger           as FL
import           System.Random                   (StdGen, newStdGen)
import           Text.Blaze.Html.Renderer.Text   (renderHtml)
import qualified Uplcg.CookieSocket              as CookieSocket
import           Uplcg.Game
import           Uplcg.Messages
import qualified Uplcg.Views                     as Views
import qualified Web.Scotty                      as Scotty

newtype RoomId = RoomId {unRoomId :: T.Text}
    deriving (Eq, Hashable, FL.ToLogStr)

data RoomPassword = NoRoomPassword | RoomPassword T.Text deriving (Eq)

type Sink = BL.ByteString -> IO ()

data Room = Room
    { roomId       :: !RoomId
    , roomPassword :: !RoomPassword
    , roomGame     :: !(TVar Game)
    , roomSinks    :: !(TVar (HMS.HashMap PlayerId Sink))
    }

data Server = Server
    { serverLogger       :: FL.FastLogger
    , serverCookieSocket :: CookieSocket.Handle Player
    , serverCards        :: Cards
    , serverRooms        :: MVar (HMS.HashMap RoomId Room)
    }

readCards :: IO Cards
readCards = Cards
    <$> fmap parseCards (T.readFile "assets/black.txt")
    <*> fmap parseCards (T.readFile "assets/white.txt")
  where
    parseCards  = V.fromList . filter (not . T.null) . map dropComment . T.lines
    dropComment = T.strip . fst . T.break (== '#')

withServer :: FL.FastLogger -> (Server -> IO a) -> IO a
withServer fl f = CookieSocket.withHandle 5 $ \cs -> do
    f =<< Server fl cs <$> readCards <*> MVar.newMVar HMS.empty

newRoom :: RoomId -> RoomPassword -> Cards -> StdGen -> STM Room
newRoom rid rpw cards gen = Room rid rpw
    <$> STM.newTVar (newGame cards gen)
    <*> STM.newTVar HMS.empty

parseRoomId :: T.Text -> Either String RoomId
parseRoomId txt
    | not (T.all isAlphaNum txt) = Left "RoomId: alphanum characters only"
    | l < 6                      = Left "RoomId: minimum length of 6"
    | l > 32                     = Left "RoomId: maximum length of 32"
    | otherwise                  = Right $ RoomId txt
  where
    l = T.length txt

parseRoomPassword :: T.Text -> Either String RoomPassword
parseRoomPassword txt
    | T.null (T.strip txt) = Right NoRoomPassword
    | T.length txt > 32    = Left "Password too long"
    | otherwise            = Right $ RoomPassword txt

instance Scotty.Parsable RoomId where
    parseParam = first TL.pack . parseRoomId . TL.toStrict

instance Scotty.Parsable RoomPassword where
    parseParam = first TL.pack . parseRoomPassword . TL.toStrict

roomViews :: Server -> IO [Views.RoomView]
roomViews server = do
    rooms <- liftIO . MVar.readMVar $ serverRooms server
    liftIO . for (HMS.toList rooms) $ \(RoomId rid, room) -> do
        sinks <- atomically . STM.readTVar $ roomSinks room
        pure $ Views.RoomView rid
            (roomPassword room /= NoRoomPassword) (HMS.size sinks)

getParam :: Scotty.Parsable a => TL.Text -> Scotty.ActionM a
getParam key =
    lookupParam key >>=
    maybe (Scotty.raise $ "Param " <> key <> " is missing") pure

lookupParam :: Scotty.Parsable a => TL.Text -> Scotty.ActionM (Maybe a)
lookupParam key = do
    params <- Scotty.params
    case lookup key params of
        Nothing -> pure Nothing
        Just value -> case Scotty.parseParam value of
             Left err -> Scotty.raise $ "Error parsing " <> key <> ": " <> err
             Right x -> pure $ Just x

getPassword :: Scotty.ActionM (Maybe T.Text)
getPassword = do
    mbPassword <- lookupParam "password"
    case mbPassword of
        Just pwd -> pure $ Just pwd
        Nothing -> do
            mbAuthorization <- Scotty.header "Authorization"
            pure $ case mbAuthorization of
                Just authorization | Just pwd <- basic authorization ->
                    Just pwd
                _ -> Nothing
  where
    basic = fmap (T.decodeUtf8 . snd) . HttpAuth.extractBasicAuth .
        T.encodeUtf8 . TL.toStrict

scottyApp :: Server -> IO Wai.Application
scottyApp server = Scotty.scottyApp $ do
    Scotty.get "/" $ Scotty.redirect $ "/rooms"

    Scotty.get "/rooms" $ do
        views <- liftIO $ roomViews server
        Scotty.html . renderHtml $ Views.rooms views

    Scotty.post "/rooms" $ do
        rid <- getParam "id"
        rpw <- getParam "password"
        _   <- liftIO $ createRoom server rid rpw
        Scotty.redirect $ TL.fromStrict $
            "/rooms/" <> unRoomId rid <>
            case rpw of
                NoRoomPassword -> ""
                RoomPassword pwd -> T.decodeUtf8 $ HttpUri.renderQuery True
                    [("password", Just $ T.encodeUtf8 pwd)]

    Scotty.get "/rooms/:id" $ do
        rid@(RoomId ridt) <- getParam "id"
        room <- liftIO $ getRoom server rid
        case roomPassword room of
            RoomPassword actual -> do
                mbGiven <- getPassword
                case mbGiven of
                    Just given | given == actual ->
                        Scotty.html . renderHtml $ Views.client ridt $ Just actual
                    _ -> do
                        Scotty.status HttpStatus.unauthorized401
                        Scotty.setHeader "WWW-Authenticate" "Basic realm=\"Provide password, user is ignored\", charset=\"UTF-8\""
            NoRoomPassword ->
                Scotty.html . renderHtml $ Views.client ridt Nothing

    Scotty.get "/assets/client.js" $ do
        Scotty.setHeader "Content-Type" "application/JavaScript"
        Scotty.file "assets/client.js"

    Scotty.get "/assets/style.css" $ do
        Scotty.setHeader "Content-Type" "text/css"
        Scotty.file "assets/style.css"

parsePendingConnection :: WS.PendingConnection -> Maybe (RoomId, RoomPassword)
parsePendingConnection pending =
    let path = WS.requestPath $ WS.pendingRequest pending
        (pathPart, queryPart) = second (B.drop 1) $ BC.break (== '?') path
        pwd = fmap T.decodeUtf8 .
            lookup "password" $ HttpUri.parseSimpleQuery queryPart in
    case filter (not . T.null) . T.split (== '/') $ T.decodeUtf8 pathPart of
        ["rooms", txt, "events"] | Right r <- parseRoomId txt ->
            Just (r, maybe NoRoomPassword RoomPassword pwd)
        _ -> Nothing

createRoom :: Server -> RoomId -> RoomPassword -> IO Room
createRoom server rid rpw = MVar.modifyMVar (serverRooms server) $ \rooms ->
    case HMS.lookup rid rooms of
        Just _ -> fail "Room already exists"
        Nothing -> do
            gen <- newStdGen
            serverLogger server $ "[" <> FL.toLogStr rid <> "] Created " <>
                (if rpw == NoRoomPassword then "" else "password-protected ") <>
                "room"
            room <- atomically $ newRoom rid rpw (serverCards server) gen
            pure (HMS.insert rid room rooms, room)

getRoom :: Server -> RoomId -> IO Room
getRoom server rid = do
    rooms <- MVar.readMVar (serverRooms server)
    case HMS.lookup rid rooms of
        Just room -> pure room
        Nothing   -> fail $ "Unknown room: " <> T.unpack (unRoomId rid)

deleteRoom :: Server -> RoomId -> IO ()
deleteRoom server rid = do
    serverLogger server $ "[" <> FL.toLogStr rid <> "] Deleting room"
    MVar.modifyMVar_ (serverRooms server) $ pure . HMS.delete rid

joinRoom :: Room -> Sink -> Maybe Player -> STM PlayerId
joinRoom room sink mbRecovered = do
    pid <- STM.stateTVar (roomGame room) $ joinGame mbRecovered
    STM.modifyTVar' (roomSinks room) $ HMS.insert pid sink
    pure pid

leaveRoom :: Room -> PlayerId -> STM (Bool, Maybe Player)
leaveRoom room pid = do
    player <- STM.stateTVar (roomGame room) $ leaveGame pid
    STM.stateTVar (roomSinks room) $ \sinks ->
        let sinks' = HMS.delete pid sinks in
        ((HMS.null sinks', player), sinks')

syncRoom :: Server -> Room -> IO ()
syncRoom server room = do
    (game, sinks) <- atomically $ (,)
        <$> STM.stateTVar (roomGame room) (\g -> (g, g & gameLog .~ []))
        <*> STM.readTVar (roomSinks room)
    for_ (reverse $ game ^. gameLog) $ \msg ->
        serverLogger server $ "[" <> FL.toLogStr (roomId room) <> "] " <>
        FL.toLogStr msg
    for_ (HMS.toList sinks) $ \(pid, sink) -> do
        let view = gameViewForPlayer pid game
        sink . Aeson.encode $ SyncGameView view

wsApp :: Server -> WS.ServerApp
wsApp server pc = case parsePendingConnection pc of
    Nothing -> WS.rejectRequest pc "Invalid URL"
    Just (rid@(RoomId ridt), givenPassword) -> do
        room <- getRoom server rid
        case roomPassword room of
            actual@(RoomPassword _) | actual /= givenPassword ->
                fail "Unauthorized"
            _ -> pure ()
        (conn, secret, mbRecovered) <-
            CookieSocket.acceptRequest (serverCookieSocket server) ridt pc
        let sink = WS.sendTextData conn
        WS.withPingThread conn 30 (pure ()) $ bracket
            (do
                pid <- atomically $ joinRoom room sink mbRecovered
                serverLogger server $ "[" <> FL.toLogStr rid <>
                    "] Player " <> FL.toLogStr pid <>
                    if isNothing mbRecovered then " joined" else " rejoined"
                pure pid)
            (\pid -> do
                (roomEmpty, mbPlayer) <- atomically $ leaveRoom room pid
                serverLogger server $ "[" <> FL.toLogStr rid <>
                    "] Player " <> FL.toLogStr pid <> " left"
                if roomEmpty
                    then deleteRoom server rid
                    else do
                        for_ mbPlayer $ CookieSocket.persist
                            (serverCookieSocket server) secret
                        syncRoom server room)
            (\playerId -> do
                sink . Aeson.encode $ Welcome ridt
                syncRoom server room
                cards <- fmap (^. gameCards) . atomically . STM.readTVar $
                    roomGame room
                sink . Aeson.encode $ SyncCards cards
                loop conn rid playerId)
  where
    loop conn rid playerId = forever $ do
        msg <- WS.receiveData conn
        case Aeson.decode msg of
            Just cm -> do
                room <- getRoom server rid
                atomically . STM.modifyTVar' (roomGame room) $
                    processClientMessage playerId cm
                syncRoom server room
            Nothing -> do
                serverLogger server $ "Could not decode client message: " <>
                    FL.toLogStr (show msg)

main :: IO ()
main = do
    host <- fromString <$> getEnv "UPLCG_HOSTNAME"
    port <- read <$> getEnv "UPLCG_PORT"
    let settings = Warp.setPort port $ Warp.setHost host Warp.defaultSettings
    timeCache <- FL.newTimeCache FL.simpleTimeFormat
    FL.withTimedFastLogger timeCache
            (FL.LogStderr FL.defaultBufSize) $ \tfl ->
        let fl s = tfl (\time -> FL.toLogStr time <> " " <> s <> "\n") in
        withServer fl $ \server -> do
        sapp <- scottyApp server
        Warp.runSettings settings $
            WaiWs.websocketsOr WS.defaultConnectionOptions (wsApp server) sapp
