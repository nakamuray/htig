{-# LANGUAGE CPP, MultiParamTypeClasses, OverloadedStrings #-}
module HTIG.Session
    ( HSession(HSession)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Data.Binary (decodeFile, encodeFile)
import Data.List (delete)
import Data.Maybe (isJust)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set

import HTIG.Core
import HTIG.Channel.OAuth
import HTIG.Database
import HTIG.IRCServer
import HTIG.TwitterAPI
import HTIG.Utils

#include "../debug.hs"


data HSession = HSession

instance ISession HSession GlobalState SessionState where
    handleClose _ = do
        whenHasNickAndUser $ do
            Just nick <- sNick <$> getLocal
            chans <- sJoinedChannels <$> getLocal
            forM_ chans $ \chan -> quitChannel chan (Nick nick) (Just "Connection closed")
            killAllH
        whenM (isJust . sNick <$> getLocal) $ do
            Just nick <- sNick <$> getLocal
            modifyGlobal $ \g ->
                let ns = gsNicks g
                in g { gsNicks = delete nick ns }

    handlePing _ sn msn = writeServerCommand $ PongCmd sn msn

    handlePass _ p = modifyLocal $ \s -> s { sPassword = Just p }

    handleNick _ n _ = do
        debug n
        ret <- modifyGlobal' $ \g ->
            let ns = gsNicks g
            in if n `elem` ns
                 then (g, False)
                 else (g { gsNicks = n:ns }, True)
        if ret
          then do
            modifyLocal $ \s -> s { sNick = Just n }
            whenHasNickAndUser $ do
                debug "nick and user sended"
                sendWelcome
                doOAuthAuthentication
                openUserDatabase
                runSessionInitHook
          else do
            writeServerError eRR_NICKNAMEINUSE $ "nickname \"" ++. n ++. "\" already exists"
            closeConn'

    handleUser _ username hostname servername realname = do
        modifyLocal $ \s -> s { sUserName   = Just username
                              , sHostName   = Just hostname
                              , sServerName = Just servername
                              , sRealName   = Just realname
                              }
        whenHasNickAndUser $ do
            debug "nick and user sended"
            sendWelcome
            doOAuthAuthentication
            openUserDatabase
            runSessionInitHook

    handleJoin _ chans = whenOAuthVerified $ do
        debug chans
        Just nick <- sNick <$> getLocal
        forM_ (Map.keys chans) $ \cname -> do
            mchan <- lookupJoinedChan cname
            case mchan of
                Just chan -> joinChannel chan (Nick nick)
                Nothing -> createNewChannel cname

    -- TODO: implement
    --handleKick _ chans nicks marg = undefined

    handlePrivmsg _ targets arg = whenOAuthVerified $ handlePrivmsg'
      where
        handlePrivmsg'
            | "\x01\&ACTION " `B.isPrefixOf` arg && "\x01" `B.isSuffixOf` arg =
                doAction targets $ stripAction arg
            | otherwise = do
                Just nick <- sNick <$> getLocal
                chans <- sJoinedChannels <$> getLocal
                forM_ chans $ \c ->
                    when (channelName c `Set.member` targets) $
                        privmsgChannel c (Nick nick) arg

        stripAction = B.init . B.tail . B.dropWhile (/= ' ')

    handlePart _ chans marg = whenHasNickAndUser $ do
        debug chans
        Just nick <- sNick <$> getLocal
        forM_ (Set.toList chans) $ \cname -> do
            mchan <- lookupJoinedChan cname
            case mchan of
                Just chan -> do
                    partChannel chan (Nick nick)
                    modifyLocal $ \s -> s { sJoinedChannels = filter ((/= cname) . channelName) $ sJoinedChannels s }
                    writeConn' $ Message (Just $ Nick nick) $ PartCmd (Set.singleton cname) Nothing
                    debug ("part from" :: String, cname)
                Nothing   -> return ()

    -- TODO: implement
    --handleCommand "INVITE" _ chans arg = undefined


lookupJoinedChan :: ChannelName -> HTIG (Maybe HChannel)
lookupJoinedChan cname = do
    chans <- sJoinedChannels <$> getLocal
    case filter ((cname == ) . channelName) chans of
        chan:_ -> return $ Just chan
        []     -> return Nothing

createNewChannel :: ChannelName -> HTIG ()
createNewChannel cname = do
    Just nick <- sNick <$> getLocal
    mchanf <- lookupChannelFactory cname
    case mchanf of
        Just chanf -> do
            chan <- chanf cname
            modifyLocal $ \s -> s { sJoinedChannels = chan : sJoinedChannels s }
            debug ("create channel for" :: String, cname)
            joinChannel chan (Nick nick)
        Nothing -> do
            -- TODO: return proper error
            debug ("no channel found" :: String, cname)
            return ()

doAction :: Set.Set TargetName -> B.ByteString -> HTIG ()
doAction ts arg = do
    debug (ts, arg)
    let (actName, arg') = B.break (== ' ') arg
    acts <- actions . gsHConfig <$> getGlobal
    case lookup actName acts of
        Just act -> act ts arg'
        Nothing  -> showActionHelp ts

showActionHelp :: Set.Set TargetName -> HTIG ()
showActionHelp ts = do
    actNames <- map fst . actions . gsHConfig <$> getGlobal
    writeServerCommand $ NoticeCmd ts "[htig] CTCP ACTION COMMANDS:"
    forM_ actNames $ \a ->
        writeServerCommand $ NoticeCmd ts a

whenHasNickAndUser :: HTIG () -> HTIG ()
whenHasNickAndUser f = do
    mn <- sNick <$> getLocal
    mu <- sUserName <$> getLocal
    when (isJust $ mn >> mu) f

whenOAuthVerified :: HTIG () -> HTIG ()
whenOAuthVerified f = whenM (isJust . sToken <$> getLocal) f

doOAuthAuthentication :: HTIG ()
doOAuthAuthentication = do
    mtok <- loadTwitterToken
    case mtok of
        Just tok -> do
            modifyLocal $ \s -> s { sToken = Just tok }
        Nothing -> do
            Just nick <- sNick <$> getLocal
            chan <- oauthChannel "#oauth"
            modifyLocal $ \s -> s { sJoinedChannels = chan : sJoinedChannels s }
            joinChannel chan (Nick nick)

runSessionInitHook :: HTIG ()
runSessionInitHook = sessionInitHook =<< gsHConfig <$> getGlobal

openUserDatabase :: HTIG ()
openUserDatabase = do
    cp <- getUserCacheDir
    let dp = cp </> "cache.sqlite"
    debug ("opening database" :: String, dp)
    conn <- liftIO $ openDatabase dp
    modifyLocal $ \s -> s { sDBConn = Just conn }

loadTwitterToken :: HTIG (Maybe Token)
loadTwitterToken = do
    makeUserCacheDir
    cpath <- getUserCacheDir
    let tpath = cpath </> "token"
    fexists <- liftIO $ doesFileExist tpath
    if fexists
      then Just <$> liftIO (decodeFile tpath)
      else return Nothing
