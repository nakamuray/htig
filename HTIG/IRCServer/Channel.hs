{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
module HTIG.IRCServer.Channel
    ( IChannel(..)
    , Channel(Channel)
    , channelName
    , joinChannel
    , kickChannel
    , noticeChannel
    , partChannel
    , privmsgChannel
    , quitChannel
    , topicChannel
    ) where

import Network.FastIRC.Types
import Network.FastIRC.Users (UserSpec)

import HTIG.IRCServer.Core

class IChannel a g l where
    onJoin :: a -> ChannelName -> UserSpec -> IRCM g l ()
    onJoin _ _ _ = return ()

    onKick :: a -> ChannelName -> UserSpec -> NickName -> CommandArg -> IRCM g l ()
    onKick _ _ _ _ _ = return ()

    onNotice :: a -> ChannelName -> UserSpec -> CommandArg -> IRCM g l ()
    onNotice _ _ _ _ = return ()

    onPart :: a -> ChannelName -> UserSpec -> IRCM g l ()
    onPart _ _ _ = return ()

    onPrivmsg :: a -> ChannelName -> UserSpec -> CommandArg -> IRCM g l ()
    onPrivmsg _ _ _ _ = return ()

    onQuit :: a -> ChannelName -> UserSpec -> Maybe CommandArg -> IRCM g l ()
    onQuit _ _ _ _ = return ()

    onTopic :: a -> ChannelName -> UserSpec -> CommandArg -> IRCM g l ()
    onTopic _ _ _ _ = return ()

data Channel g l = forall a. IChannel a g l => Channel a ChannelName

channelName :: Channel g l -> ChannelName
channelName (Channel _ n) = n

joinChannel :: Channel g l -> UserSpec -> IRCM g l ()
joinChannel (Channel a n) = onJoin a n

kickChannel :: Channel g l -> UserSpec -> NickName -> CommandArg -> IRCM g l ()
kickChannel (Channel a n) = onKick a n

noticeChannel :: Channel g l -> UserSpec -> CommandArg -> IRCM g l ()
noticeChannel (Channel a n) = onNotice a n

partChannel :: Channel g l -> UserSpec -> IRCM g l ()
partChannel (Channel a n) u = onPart a n u

privmsgChannel :: Channel g l -> UserSpec -> CommandArg -> IRCM g l ()
privmsgChannel (Channel a n) u m = onPrivmsg a n u m

quitChannel :: Channel g l -> UserSpec -> Maybe CommandArg -> IRCM g l ()
quitChannel (Channel a n) = onQuit a n

topicChannel :: Channel g l -> UserSpec -> CommandArg -> IRCM g l ()
topicChannel (Channel a n) = onTopic a n
