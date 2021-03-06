{-# LANGUAGE PatternGuards #-}
-- An EDSL for StatusHooks
-- Largely inspired by (and copied from) XMonad.ManageHook

module HTIG.StatusHook where

import Control.Monad
import Control.Monad.Trans
import Data.Char (chr, isDigit)
import Data.Monoid
import Network.FastIRC.Types (ChannelName)

import HTIG.Core
import HTIG.TwitterAPI


-- | Lift an 'HTIG' action to a 'Query'.
liftH :: HTIG a -> Query a
liftH = Query . lift

-- | The identity hook that returns the Status unchanged.
idHook :: StatusHook
idHook = doF id

-- | Infix 'mappend'. Compose two 'StatusHook' from right to left.
(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

-- | Compose the list of 'StatusHook's.
composeAll :: [StatusHook] -> StatusHook
composeAll = mconcat

infix 0 -->

-- | @p --> x@.  If @p@ returns 'True', execute the 'StatusHook'.
(-->) :: Query Bool -> StatusHook -> StatusHook
p --> f = p >>= \b -> if b then f else mempty

-- | @q =? x@. if the result of @q@ equals @x@, return 'True'.
(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = fmap (== x) q

infixr 3 <&&>, <||>

-- | '&&' lifted to a 'Monad'.
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

-- | '||' lifted to a 'Monad'.
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)


isPrivMsg, isNoticeMsg :: Query Bool
isPrivMsg   = asks $ (== PrivMsg) . cMsgType
isNoticeMsg = asks $ (== NoticeMsg) . cMsgType

channelIs :: ChannelName -> Query Bool
channelIs cname = asks $ (== cname) . cChan

doH :: (HTIG (Maybe Status) -> HTIG (Maybe Status)) -> StatusHook
doH = return . Endo

doH' :: (Status -> HTIG (Maybe Status)) -> StatusHook
doH' f = doH $ \h -> do
    ret <- h
    case ret of
        Just st -> f st
        Nothing -> return Nothing

doF :: (Maybe Status -> Maybe Status) -> StatusHook
doF = doH . fmap

doF' :: (Status -> Status) -> StatusHook
doF' = doF . fmap

doT :: (String -> String) -> StatusHook
doT f = doF' $ \st -> st { stText = f $ stText st }


-- | StatusHook that unescape "&amp;", "&gt;", "&lt;" and "&quot;"
doUnHtmlEscape :: StatusHook
doUnHtmlEscape = doT unHtmlEscape

unHtmlEscape :: String -> String
unHtmlEscape ('&':'a':'m':'p':';':cs)     = '&' : unHtmlEscape cs
unHtmlEscape ('&':'g':'t':';':cs)         = '>' : unHtmlEscape cs
unHtmlEscape ('&':'l':'t':';':cs)         = '<' : unHtmlEscape cs
unHtmlEscape ('&':'q':'u':'o':'t':';':cs) = '"' : unHtmlEscape cs
unHtmlEscape (c:cs)                       = c   : unHtmlEscape cs
unHtmlEscape []                           = []


-- | StatusHook that convert HTML entity refs to unicode char
doUnHtmlEntityRef :: StatusHook
doUnHtmlEntityRef = doT unHtmlEntityRef

unHtmlEntityRef :: String -> String
unHtmlEntityRef ('&':'#':cs)
    | (h, ';':t) <- span (/=';') cs,
      all isDigit h    = chr (read h) : unHtmlEntityRef t
unHtmlEntityRef (c:cs) = c : unHtmlEntityRef cs
unHtmlEntityRef []     = []


-- | StatusHook that replace status with retweeted one,
--   prepend "♺ " to status text and append "[retweeted by screen_name]"
doShowRT :: StatusHook
doShowRT = doF' $ \st ->
    case stRetweetedStatus st of
        Just st' -> st' { stText = color 10 "\9850 " ++ stText st' ++ color 10 (" [retweeted by " ++ usScreenName (stUser st) ++ "]")
                        , stId = stId st -- set original status id
                        }
        Nothing  -> st


-- | StatusHook that prepend "↻ " to reply status text
doShowReply :: StatusHook
doShowReply = doF' $ \st ->
    case stInReplyToStatusId st of
        Just _  -> st { stText = color 10 "\8635 " ++ stText st }
        Nothing -> st


-- | filter status
doFilterStatus :: (Status -> Bool) -> StatusHook
doFilterStatus f = doF $ \ms -> do
    s <- ms
    if f s
      then Just s
      else Nothing


color :: Int -> String -> String
color cNum cs = "\x03\&" ++ show cNum ++ cs ++ "\x0f"
