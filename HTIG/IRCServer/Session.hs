{-# LANGUAGE MultiParamTypeClasses #-}
module HTIG.IRCServer.Session where

import Network.FastIRC.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

import HTIG.IRCServer.Core

class ISession s g l where
    --handleConnect :: Connection -> IRCM g l s
    handleClose :: s -> IRCM g l ()
    handleClose _ = return ()

    handlePing :: s -> ServerName -> Maybe ServerName -> IRCM g l ()
    handlePing _ _ _ = return ()


    handlePass :: s -> CommandArg -> IRCM g l ()
    handlePass _ _ = return ()

    handleNick :: s -> NickName -> Maybe Int -> IRCM g l ()
    handleNick _ _ _ = return ()

    handleUser :: s -> UserName -> CommandArg -> CommandArg -> CommandArg -> IRCM g l ()
    handleUser _ _ _ _ _ = return ()


    handleJoin :: s -> Map.Map ChannelName (Maybe ChannelKey) -> IRCM g l ()
    handleJoin _ _ = return ()

    handleKick :: s -> Set.Set ChannelName -> Set.Set NickName -> Maybe CommandArg -> IRCM g l ()
    handleKick _ _ _ _ = return ()

    handleMode :: s -> Maybe (TargetName, CommandArg, [CommandArg]) -> IRCM g l ()
    handleMode _ _ = return ()

    handleNotice :: s -> Set.Set TargetName -> CommandArg -> IRCM g l ()
    handleNotice _ _ _ = return ()

    handlePart :: s -> Set.Set ChannelName -> Maybe CommandArg -> IRCM g l ()
    handlePart _ _ _ = return ()

    handlePrivmsg :: s -> Set.Set TargetName -> CommandArg -> IRCM g l ()
    handlePrivmsg _ _ _ = return ()

    handleQuit :: s -> Maybe CommandArg -> IRCM g l ()
    handleQuit _ _ = return ()

    handleTopic :: s -> ChannelName -> Maybe CommandArg -> IRCM g l ()
    handleTopic _ _ _ = return ()


    handleCommand :: s -> CommandName -> [CommandArg] -> IRCM g l ()
    handleCommand _ _ _ = return ()
