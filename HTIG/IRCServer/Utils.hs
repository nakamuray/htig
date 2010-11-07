{-# LANGUAGE CPP #-}
module HTIG.IRCServer.Utils where

import Network.FastIRC.Messages (Message(Message), Command(NoticeCmd, PrivMsgCmd))
import Network.FastIRC.Types
import Network.FastIRC.Users (UserSpec)

import qualified Data.Set as Set

import HTIG.IRCServer.Connection
import HTIG.IRCServer.Core

writePrivMsg' :: UserSpec -> TargetName -> CommandArg -> IRCM g l ()
writePrivMsg' from to msg = do
    conn <- getConn
    writePrivMsg conn from (Set.singleton to) msg

writePrivMsg :: Connection -> UserSpec -> Set.Set TargetName -> CommandArg -> IRCM g l ()
writePrivMsg = writeMsg PrivMsgCmd

writeNotice' :: UserSpec -> TargetName -> CommandArg -> IRCM g l ()
writeNotice' from to msg = do
    conn <- getConn
    writeNotice conn from (Set.singleton to) msg

writeNotice :: Connection -> UserSpec -> Set.Set TargetName -> CommandArg -> IRCM g l ()
writeNotice = writeMsg NoticeCmd

writeMsg :: (Set.Set TargetName -> CommandArg -> Command) -> Connection -> UserSpec -> Set.Set TargetName -> CommandArg -> IRCM g l ()
writeMsg c conn from to msg = liftIO $ writeConn conn $ Message (Just from) $ c to msg

writeConn' :: Message -> IRCM g l ()
writeConn' msg = do
    conn <- getConn
    liftIO $ writeConn conn msg

closeConn' :: IRCM g l ()
closeConn' = getConn >>= liftIO . closeConn
