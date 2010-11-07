module HTIG.Action
    ( userAction
    , replyAction
    , retweetAction

    , doActionWithStatus
    , mapActionWithStatuses

    , argToTids
    ) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec

import qualified Data.Set as Set

import HTIG.Core
import HTIG.Database
import HTIG.Hooks.Tid
import HTIG.IRCServer
import HTIG.TwitterAPI
import HTIG.Utils


-- | get user's recent statuses
-- USAGE: user screen_name [num]
userAction :: Action
userAction ts arg = do
    case parse (spaces >> screenNameAndCount) "" $ s arg of
        Right (sname, mn) -> do
            Just tok <- sToken <$> getLocal
            resTL <- liftIO $ getUserTimeline tok sname mn
            case resTL of
                Ok tl -> forM_ (Set.toList ts) $ \t ->
                             forM_ tl (writeNoticeStatus t)
                Error e -> writeServerCommand $ NoticeCmd ts $ b("cannot fetch " ++ sname ++ "'s timeline: " ++ e)
        Left e ->
            writeServerCommand $ NoticeCmd ts $ b(show e)

-- | reply to status
-- USAGE: reply tid reply message
replyAction :: Action
replyAction = doActionWithStatus $ \(Status { stId = stid, stText = text, stUser = u }) msg -> do
    Just tok <- sToken <$> getLocal
    let at_screen_name = "@" ++ usScreenName u
    ret <- liftIO $ updateStatus tok (Just stid) $ at_screen_name ++ " " ++ msg
    case ret of
        Ok _    -> return $ "update status in reply to " ++ at_screen_name ++ ": " ++ snip 10 text
        Error e -> return $ "cannot reply: " ++ e

-- | retweet status
-- USAGE: reply tid
-- USAGE: reply tid retweet message
--
-- if no retweet message, use official retweet
-- otherwize unofficial retweet
retweetAction :: Action
retweetAction = doActionWithStatus $ \(Status { stId = stid, stText = text, stUser = u }) msg -> do
    Just tok <- sToken <$> getLocal
    ret <-
        if length msg > 0
          then do
            let text' = snip 140 $ msg ++ " RT @" ++ usScreenName u ++ ": " ++ text
            liftIO $ updateStatus tok Nothing text'
          else
            liftIO $ retweet tok stid
    case ret of
        Ok _    -> return $ "retweet status of @" ++ usScreenName u ++ ": " ++ snip 10 text
        Error e -> return $ "cannot retweet: " ++ e


-- | create Action that receive tid and some args, find Status from tid,
--   apply ACTION to Status and remaining args
doActionWithStatus :: (Status -> String -> HTIG String) -> Action
doActionWithStatus h = \ts arg -> do
    case argToTidAndRest $ s arg of
        Right (t, arg') -> do
            mst <- withConnection $ getStatusFromTid t
            case mst of
                Just st -> h st arg' >>= noticeToWhenLength ts
                Nothing -> noticeToWhenLength ts $ "Cannot find status from tid: " ++ t
        Left  e ->
            writeServerCommand $ NoticeCmd ts $ b(show e)

-- | create Action that receive list of tids and convert to list of Statuses,
--   map ACTION to that list.
mapActionWithStatuses :: (Status -> HTIG String) -> Action
mapActionWithStatuses h = \ts arg -> do
    let tids = argToTids $ s arg
    statuses <- catMaybes <$> mapM (withConnection . getStatusFromTid) tids
    forM_ statuses $ \st -> h st >>= noticeToWhenLength ts


noticeToWhenLength :: Set.Set TargetName -> String -> HTIG ()
noticeToWhenLength ts msg | length msg > 0 = writeServerCommand $ NoticeCmd ts $ b msg
                          | otherwise      = return ()


--
-- Parsers
--

-- | parser for args
-- "aa, ai ao aka-asa" -> ["aa", "ai", "ao", "aka", "aki", "aku", "ake", "ako", "asa"]
argToTids :: String -> [Tid]
argToTids arg = case parse (spaces >> parseTids) "" arg of
                    Right tids -> tids
                    Left  _    -> []

-- | parser for args with tid
-- "aa hello world" -> ("aa", "hello world")
argToTidAndRest :: String -> Either ParseError (Tid, String)
argToTidAndRest arg = parse (spaces >> ((, ) <$> tid <* spaces <*> many anyChar)) "" arg

parseTids :: Parser [Tid]
parseTids = concat <$> sepBy (try tidRange <|> (:[]) <$> tid) sep

sep :: Parser ()
sep = (many1 $ oneOf " ,") >> return ()

tid :: Parser Tid
tid = do
    c1 <- oneOfString $ reverse consonant -- ^ move "" to last
    v1 <- oneOfString vowel
    c2 <- oneOfString $ reverse consonant -- ^ move "" to last
    v2 <- oneOfString vowel
    return $ c1 ++ v1 ++ c2 ++ v2

oneOfString :: [String] -> Parser String
oneOfString css = foldl1 (<|>) $ map string css

tidRange :: Parser [Tid]
tidRange = do
    tidStart <- tid
    many $ char ' '
    char '-'
    many $ char ' '
    tidEnd <- tid
    return $ takeWhile (/= (nextTid tidEnd)) $ dropWhile (/= tidStart) tidTable

screenNameAndCount :: Parser (String, Maybe Int)
screenNameAndCount = do
    sname <- screenName
    spaces
    mn <- optionMaybe $ many1 digit
    return (sname, read <$> mn)

screenName :: Parser String
screenName = many1 $ letter <|> digit <|> char '_'
