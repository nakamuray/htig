module HTIG.Hooks.ShowTimeDiff ( doShowTimeDiff ) where

import Data.Time (UTCTime, getCurrentTime, diffUTCTime, utcToLocalZonedTime, formatTime)
import System.Locale (defaultTimeLocale)

import HTIG.Core
import HTIG.StatusHook
import HTIG.TwitterAPI


-- | StatusHook that display time diff like "[about 2 hours ago]", "[5 AM Aug 17th]" for old posts
doShowTimeDiff :: StatusHook
doShowTimeDiff = doH' $ \st -> do
    d <- liftIO $ prettyShowTime $ stCreatedAt st
    let text = stText st ++ d
    return $ Just $ st { stText = text }

prettyShowTime :: UTCTime -> IO String
prettyShowTime t = do
    now <- getCurrentTime
    case diffUTCTime now t of
        d | d < 60 * 60      -> return "" -- ignore recent post
          | d < 60 * 60 * 2  -> return $ color 10 $ " [about 1 hour ago]"
          | d < 60 * 60 * 24 -> return $ color 10 $ " [about " ++ show (truncate d `div` (60 * 60)) ++ " hours ago]"
          | otherwise         -> do
            t' <- utcToLocalZonedTime t
            return $ color 10 $ formatTime defaultTimeLocale " [%-l %p %b %eth]" t'
