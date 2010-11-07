{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module HTIG.Channel.Mention
    ( mention
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
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


data MentionChannel = MentionChannel
    { mentionFetcherId :: TVar (Maybe ThreadId)
    }

mention :: HChannelFactory
mention cname = do
    tv <- liftIO $ newTVarIO Nothing
    return $ Channel (MentionChannel tv) cname

instance IChannel MentionChannel GlobalState SessionState where
    onQuit (MentionChannel tv) _ _ _ = killHwhenRunning tv
    onPart c _ _ = onQuit c undefined undefined undefined

    onJoin (MentionChannel tv) cname u = do
        writeJoin u cname
        forkHwhenNotRunning tv $ mentionFetcher cname

    onPrivmsg _ cname u arg = updateTwitterStatus cname arg

mentionFetcher :: ChannelName -> HTIG ()
mentionFetcher cname = do
    Just tok <- sToken <$> getLocal
    Just nick <- sNick <$> getLocal
    let nick' = s nick
    mlsid <- withConnection $ getMentionLastStatusId nick'
    resMentions <- liftIO $ getMentions tok mlsid
    case resMentions of
        Ok mentions -> do
            forM_ mentions $ \t -> do
                withTransaction $ addToMention nick' t
                writeStatus cname t
        Error e ->
            writeServerCommand $ NoticeCmd (Set.singleton cname) $ b("cannot fetch mentions: " ++ e)
    liftIO $ threadDelay $ 60 * 1000 * 1000
    mentionFetcher cname

-- XXX: these two functions are copied from HTIG.Channel.Timeline
-- TODO: make some framework to start thread for channel
--       and kill the thread when channel closed
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
