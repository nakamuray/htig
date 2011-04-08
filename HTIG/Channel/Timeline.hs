{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module HTIG.Channel.Timeline
    ( timeline
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad (forM_, when)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)

import qualified Data.Map as Map
import qualified Data.Set as Set

import HTIG.Core
import HTIG.Database
import HTIG.IRCServer
import HTIG.TwitterAPI
import HTIG.Utils


data TimelineChannel = TimelineChannel
    { timelineFetcherId :: TVar (Maybe ThreadId)
    , friendsFetcherId :: TVar (Maybe ThreadId)
    , userStreamFetcherId :: TVar (Maybe ThreadId)
    , userStreamChannel :: TChan Status
    }

timeline :: HChannelFactory
timeline cname = do
    ttl <- liftIO $ newTVarIO Nothing
    tf <- liftIO $ newTVarIO Nothing
    tu <- liftIO $ newTVarIO Nothing
    chan <- liftIO $ newTChanIO
    return $ Channel (TimelineChannel ttl tf tu chan) cname

instance IChannel TimelineChannel GlobalState SessionState where
    onQuit (TimelineChannel ttl tf tu _) _ _ _ = do
        killHwhenRunning ttl
        killHwhenRunning tf
        killHwhenRunning tu

    onPart c _ _ = onQuit c undefined undefined undefined

    onJoin (TimelineChannel ttl tf tu chan) cname u = do
        writeJoin u cname
        Just tok <- sToken <$> getLocal
        Just nick <- sNick <$> getLocal

        resLastSt <- liftIO $ getLastStatus tok
        case resLastSt of
            Ok lastSt ->
                writeServerCommand $ NumericCmd rPL_TOPIC $ [nick, cname, b(stText lastSt)]
            Error e ->
                writeServerCommand $ NoticeCmd (Set.singleton cname) $ b("cannot fetch your last status: " ++ e)

        resFriends <- liftIO $ getFriends tok $ s nick
        case resFriends of
            Ok friends -> do
                writeServerCommand $ NumericCmd rPL_NAMREPLY $ [nick, cname, b(intercalate " " $ map usScreenName friends)]
                writeServerCommand $ NumericCmd rPL_ENDOFNAMES $ [nick, cname, "End of /NAMES list"]
            Error e ->
                writeServerCommand $ NoticeCmd (Set.singleton cname) $ b("cannot fetch friends: " ++ e)

        forkHwhenNotRunning ttl $ showRecentTimeline cname >> homeTimelineFetcher cname chan
        forkHwhenNotRunning tf $ friendsFetcher cname
        forkHwhenNotRunning tu $ userStreamFetcher chan

    onPrivmsg _ cname u arg = updateTwitterStatus cname arg

forkHwhenNotRunning :: TVar (Maybe ThreadId) -> HTIG () -> HTIG ()
forkHwhenNotRunning t h = do
    isRunning <- liftIO $ atomically $ isJust <$> readTVar t
    when (not isRunning) $ do
        i <- forkH $ h
        liftIO $ atomically $ writeTVar t (Just i)

killHwhenRunning :: TVar (Maybe ThreadId) -> HTIG ()
killHwhenRunning t = do
        mtid <- liftIO $ atomically $ readTVar t
        case mtid of
            Just tid -> do
                killH tid
                liftIO $ atomically $ writeTVar t Nothing
            Nothing  -> return ()

showRecentTimeline ::ChannelName ->  HTIG ()
showRecentTimeline cname = do
    Just tok <- sToken <$> getLocal
    Just nick <- sNick <$> getLocal
    let nick' = s nick
    mlsid <- withConnection $ getTLLastStatusId nick'
    resTl <- liftIO $ getHomeTimeline tok mlsid
    case resTl of
        Ok tl -> do
            forM_ tl $ \t -> do
                --debug t
                withTransaction $ addToTimeline nick' t
                writeStatus cname t
        Error e ->
            writeServerCommand $ NoticeCmd (Set.singleton cname) $ b("cannot fetch timeline: " ++ e)


homeTimelineFetcher :: ChannelName -> TChan Status -> HTIG ()
homeTimelineFetcher cname chan = do
    Just tok <- sToken <$> getLocal
    Just nick <- sNick <$> getLocal
    let nick' = s nick
    st <- liftIO $ atomically $ readTChan chan
    withTransaction $ addToTimeline nick' st
    writeStatus cname st
    homeTimelineFetcher cname chan

friendsFetcher :: ChannelName -> HTIG ()
friendsFetcher cname = do
    Just tok <- sToken <$> getLocal
    Just nick <- sNick <$> getLocal
    let nick' = s nick
    resFriends <- liftIO $ getFriends tok nick'
    case resFriends of
        Ok friends ->
            friendsFetcher' cname friends
        Error e -> do
            writeServerCommand $ NoticeCmd (Set.singleton cname) $ b("cannot fetch friends: " ++ e)
            friendsFetcher cname

friendsFetcher' :: ChannelName -> [TwitterUser] -> HTIG ()
friendsFetcher' cname friends = do
    liftIO $ threadDelay $ 10 * 60 * 1000 * 1000
    Just tok <- sToken <$> getLocal
    Just nick <- sNick <$> getLocal
    let nick' = s nick
    resFriends <- liftIO $ getFriends tok nick'
    case resFriends of
        Ok newFriends -> do
            let friends' = Set.fromList $ map usScreenName friends
                newFriends' = Set.fromList $ map usScreenName newFriends
                joined = Set.difference newFriends' friends'
                parted = Set.difference friends' newFriends'
            forM_ (Set.toList parted) $ \f ->
                writeConn' $ Message (Just $ Nick (b f)) $ PartCmd (Set.singleton cname) Nothing
            forM_ (Set.toList joined) $ \f ->
                writeConn' $ Message (Just $ Nick (b f)) $ JoinCmd $ Map.fromList [(cname, Nothing)]
            friendsFetcher' cname newFriends
        Error e -> do
            writeServerCommand $ NoticeCmd (Set.singleton cname) $ b("cannot fetch friends: " ++ e)
            friendsFetcher' cname friends

userStreamFetcher :: TChan Status -> HTIG ()
userStreamFetcher chan = do
    Just tok <- sToken <$> getLocal
    liftIO $ userStreamFetcher' tok chan
    userStreamFetcher chan

userStreamFetcher' :: Token -> TChan Status -> IO (Result ())
userStreamFetcher' tok chan = userStreamIter tok $ atomically . writeTChan chan
