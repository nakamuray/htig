{-# LANGUAGE CPP, ExistentialQuantification #-}
module HTIG.IRCServer.Main
    ( Server(..)
    , runServer
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Network.FastIRC.Messages (Message(msgCommand), Command(..))
import Network.Socket (Socket, socket, Family(AF_INET), SocketType(Stream), SocketOption(ReuseAddr),
                       defaultProtocol, SockAddr(SockAddrInet), bindSocket, inet_addr, listen, setSocketOption)

import HTIG.IRCServer.Connection
import HTIG.IRCServer.Core
import HTIG.IRCServer.Session

#include "../../debug.hs"


data Server s g l = Server
    { host :: String
    , port :: Int
    , initGlobal :: IO g
    , initLocal :: IO l
    -- TODO: use Addr to init Session
    , initSession :: Connection -> IO s
    }


runServer :: (ISession s g l) => Server s g l -> IO ()
runServer srv = do
    debug "starting server ..."
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    debug (host srv, port srv)
    host' <- inet_addr $ host srv
    let port' = fromIntegral $ port srv
    bindSocket sock (SockAddrInet port' host')
    listen sock 10
    g <- newTVarIO =<< initGlobal srv
    mainLoop srv g sock

mainLoop :: (ISession s g l) => Server s g l -> TVar g -> Socket -> IO ()
mainLoop srv g sock = do
    conn <- acceptConn sock
    debug ("connection accepted" :: String, conn)
    forkIO $ runSession g srv conn
    mainLoop srv g sock

runSession :: (ISession s g l) => TVar g -> Server s g l -> Connection -> IO ()
runSession g srv conn = do
    debug ("start session for" :: String, conn)
    l <- newTVarIO =<< initLocal srv
    s <- initSession srv conn
    -- TODO: handle exception
    runIRCM (runSession' s) g l conn

runSession' :: (ISession s g l) => s -> IRCM g l ()
runSession' s = do
    conn <- getConn
    mmsg <- liftIO $ readConn conn
    debug mmsg
    case mmsg of
        Just msg -> do
            case msgCommand msg of
                PingCmd sn msn -> handlePing s sn msn
                PassCmd a -> handlePass s a
                NickCmd n mi -> handleNick s n mi
                UserCmd n a1 a2 a3 -> handleUser s n a1 a2 a3
                JoinCmd cs -> handleJoin s cs
                KickCmd cs ns ma -> handleKick s cs ns ma
                ModeCmd marg -> handleMode s marg
                NoticeCmd ts a -> handleNotice s ts a
                PartCmd cs ma -> handlePart s cs ma
                PrivMsgCmd ts a -> handlePrivmsg s ts a
                QuitCmd ma -> handleQuit s ma
                TopicCmd c ma -> handleTopic s c ma
                StringCmd c as -> handleCommand s c as
                -- ignore these commands
                PongCmd _ _ -> return ()
                NumericCmd _ _ -> return ()
            runSession' s

        Nothing -> do
            handleClose s
            debug ("connection closed, stop runSession'" :: String)
