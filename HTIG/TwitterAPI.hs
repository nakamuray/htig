{-# LANGUAGE CPP, OverloadedStrings, PatternGuards, TupleSections, Rank2Types #-}
module HTIG.TwitterAPI
    ( Timeline
    , Status(..)
    , TwitterUser(..)

    , getHomeTimeline
    , getMentions
    , getFriends
    , getLastStatus
    , updateStatus
    , followScreenName
    , unFollowScreenName
    , retweet
    , getUserTimeline
    , getRateLimit
    , userStreamIter
    , doOAuth

    -- re-export for convention
    , Result(Ok, Error)
    , Token
    ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromJust, maybeToList)
import Data.Time (UTCTime, parseTime)
import Network.OAuth.Consumer (Application, Token(application), SigMethod(HMACSHA1), OAuthMonadT,
                               oauthParams, runOAuthM, fromApplication, signRq2, oauthRequest, getToken, putToken,
                               serviceRequest, injectOAuthVerifier)
import HTIG.EnumHttpClient (EnumHttpClient(EnumHttpClient, EnumHttpClientWithIter), EnumResponseHandler)
import Network.OAuth.Http.Request (Request(method, pathComps, qString), Method(GET, POST),
                                   findWithDefault, parseURL, fromList, union)
import Network.OAuth.Http.Response (Response(status, reason, rspPayload))
import Network.OAuth.Http.Util (splitBy)
import System.Locale (defaultTimeLocale)
import Text.JSON (JSValue(JSObject, JSNull), Result(Ok, Error), JSON, JSObject, decode, readJSON, valFromObj)

import qualified Data.ByteString.Lazy.UTF8 as BU
import qualified Data.ByteString.UTF8 as BSU

import Network.HTTP.Enumerator (Response(Response, responseBody))
import qualified Network.HTTP.Types as W
import Data.Enumerator ((=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL


#include "../debug.hs"


--
-- | Twitter API types
--

twitterAPIURL :: String
twitterAPIURL = "https://api.twitter.com/"

twitterAPIVersion :: Int
twitterAPIVersion = 1

twitterStreamAPIURL :: String
twitterStreamAPIURL = "https://userstream.twitter.com/"

twitterStreamAPIVersion :: Int
twitterStreamAPIVersion = 2


type Timeline = [Status]

data Status = Status
    { stCreatedAt :: UTCTime
    , stId :: Integer
    , stText :: String
    , stSource :: String
    , stTruncated :: Bool
    , stInReplyToStatusId :: Maybe Integer
    , stInReplyToUserId :: Maybe Integer
    , stFavorited :: Bool
    , stInReplyToScreenName :: Maybe String
    , stUser :: TwitterUser
    --, stEntities :: Entities
    , stRetweetedStatus :: Maybe Status
    } deriving (Show, Read, Eq)

data TwitterUser = TwitterUser
    { usId :: Integer
    , usName :: String
    , usScreenName :: String
    -- TODO: other user attributes
    }
    | TwitterUserId
    { usId :: Integer
    } deriving (Show, Read, Eq, Ord)

data Entities = Entities
    { enUrls  :: [UrlEntity]
    -- TODO:
    --, enMedia :: [MediaEntity]
    --, enUserMentions :: [UserMentionEntity]
    --, enHashtags :: [HashtagEntiry]
    } deriving (Show, Read, Eq)

data UrlEntity = UrlEntity
    { ueUrl         :: String
    , ueDisplayUrl  :: Maybe String
    , ueExpandedUrl :: Maybe String
    , ueIndices     :: (Int, Int)
    } deriving (Show, Read, Eq)


decodeTimeline :: JSValue -> Result Timeline
decodeTimeline input = readJSON input >>= mapM decodeStatus >>= return . sortBy (compare `on` stId)


decodeStatus :: JSValue -> Result Status
decodeStatus v@(JSObject obj) = do
    st <- decodeStatus' v
    case decodeEntities =<< valFromObj "entities" obj of
        Ok Entities { enUrls = ues } -> return st { stText = expandUrls ues $ stText st }
        _ -> return st
decodeStatus j = Error $ "invalid JSValue" ++ show j


decodeStatus' :: JSValue -> Result Status
decodeStatus' (JSObject obj) = Status <$> (maybeToResult . parseTime' =<< valFromObj "created_at" obj)
                                      <*> valFromObj "id" obj
                                      <*> valFromObj "text" obj
                                      <*> valFromObj "source" obj
                                      <*> valFromObj "truncated" obj
                                      <*> valMayFromObj "in_reply_to_status_id" obj
                                      <*> valMayFromObj "in_reply_to_user_id" obj
                                      <*> valFromObj "favorited" obj
                                      <*> valMayFromObj "in_reply_to_screen_name" obj
                                      <*> (decodeUser =<< valFromObj "user" obj)
                                      <*> getRetweetedStatus obj
decodeStatus' j = Error $ "invalid JSValue" ++ show j

getRetweetedStatus :: JSObject JSValue -> Result (Maybe Status)
getRetweetedStatus obj =
    case valFromObj "retweeted_status" obj of
        Ok j    -> Just <$> decodeStatus j
        Error _ -> Ok Nothing

-- | Nothing if value is null
valMayFromObj :: JSON a => String -> JSObject JSValue -> Result (Maybe a)
valMayFromObj cs obj = valFromObj cs obj >>= toMaybe
  where
    toMaybe JSNull = return Nothing
    toMaybe jv     = Just <$> readJSON jv


decodeUser :: JSValue -> Result TwitterUser
decodeUser (JSObject obj) = TwitterUser <$> valFromObj "id" obj
                                        <*> valFromObj "name" obj
                                        <*> valFromObj "screen_name" obj
                          <|>
                            TwitterUserId <$> valFromObj "id" obj
decodeUser j = Error $ "invalid JSValue" ++ show j


decodeEntities :: JSValue -> Result Entities
decodeEntities (JSObject obj) = Entities <$> (valFromObj "urls" obj >>= mapM decodeUrlEntity)
decodeEntities j = Error $ "invalid JSValue" ++ show j


decodeUrlEntity :: JSValue -> Result UrlEntity
decodeUrlEntity (JSObject obj) = UrlEntity <$> valFromObj "url" obj
                                           <*> valMayFromObj "display_url" obj
                                           <*> valMayFromObj "expanded_url" obj
                                           <*> (valFromObj "indices" obj >>= l2t)
  where
    l2t [i, j] = Ok (i, j)
    l2t xs     = Error $ "not a [i, j], but" ++ show xs

decodeUrlEntity j = Error $ "invalid JSValue" ++ show j


expandUrls :: [UrlEntity] -> String -> String
expandUrls [] text       = text
expandUrls (ue@(UrlEntity { ueExpandedUrl = Just eurl, ueIndices = (start, end) }):ues) text = expandUrls ues $ take start text ++ eurl ++ drop end text
expandUrls (_:ues) text = expandUrls ues text


parseTime' :: String -> Maybe UTCTime
parseTime' = parseTime defaultTimeLocale "%c"


maybeToResult :: Maybe a -> Result a
maybeToResult (Just a) = Ok a
maybeToResult Nothing = Error "Nothing"


-- | get home timeline
getHomeTimeline :: Token
                -> Maybe Integer -- ^ Returns results with an ID greater than this ID
                -> IO (Result Timeline)
getHomeTimeline tok mLastId = do
    let query = ("count", "20") : ("include_entities", "1") : (maybeToList $ (("since_id", ) . show) <$> mLastId)
    json <- callAPI GET tok "statuses/home_timeline" query
    return $ json >>= decodeTimeline


-- | get home mentions
getMentions :: Token
            -> Maybe Integer -- ^ Returns results with an ID greater than this ID
            -> IO (Result Timeline)
getMentions tok mLastId = do
    let query = ("count", "20") : ("include_entities", "1") : (maybeToList $ (("since_id", ) . show) <$> mLastId)
    json <- callAPI GET tok "statuses/mentions" query
    return $ json >>= decodeTimeline


-- | get friends from screen_name
getFriends :: Token
           -> String -- ^ screen name
           -> IO (Result [TwitterUser])
getFriends tok screen_name = getFriends' tok screen_name (-1)


getFriends' :: Token
            -> String  -- ^ screen name
            -> Integer -- ^ cursor
            -> IO (Result [TwitterUser])
getFriends' tok screen_name cur = do
    json <- callAPI GET tok "statuses/friends" [ ("screen_name", screen_name)
                                               , ("cursor", show cur)]
    case json >>= decodeFriendsWithCursor of
        Ok (prev, next, users) ->
            if next /= 0
              then do
                users' <- getFriends' tok screen_name next
                return $ (users ++) <$> users'
              else
                return $ Ok users
        Error e -> return $ Error e


decodeFriendsWithCursor :: JSValue -> Result (Integer, Integer, [TwitterUser])
decodeFriendsWithCursor (JSObject obj) = do
    previous_cursor <- valFromObj "previous_cursor" obj
    next_cursor <- valFromObj "next_cursor" obj
    users <- valFromObj "users" obj >>= mapM decodeUser
    return (previous_cursor, next_cursor, users)
decodeFriendsWithCursor j = Error $ "invalid JSValue" ++ show j


-- | get authenticated user's last status
getLastStatus :: Token -> IO (Result Status)
getLastStatus tok = do
    -- if last status is official RT, count=1 return empty list
    -- so use count=5
    json <- callAPI GET tok "statuses/user_timeline" [("count", "5"), ("include_entities", "1")]
    -- FIXME: crash when returned empty array
    return $ json >>= readJSON >>= decodeStatus . head


-- | update twitter status
updateStatus :: Token
             -> Maybe Integer -- ^ status id to reply
             -> String        -- ^ status message
             -> IO (Result Status)
updateStatus tok mRepId msg = do
    json <- callAPI POST tok "statuses/update" $ catMaybes [Just ("status", msg)
                                                           , ("in_reply_to_status_id", ) . show <$> mRepId]
    return $ json >>= decodeStatus


-- | follow screen_name
followScreenName :: Token
                 -> String -- ^ screen name to follow
                 -> IO (Result TwitterUser)
followScreenName tok sname = do
    ret <- callAPI POST tok "friendships/create" [("screen_name", sname), ("follow", "true")]
    return $ ret >>= decodeUser


-- | unfollow screen_name
unFollowScreenName :: Token
                   -> String -- ^ screen name to unfollow
                   -> IO (Result ())
unFollowScreenName tok sname = do
    -- XXX: is friendships/destroy return valid JSON string?
    ret <- callAPI POST tok "friendships/destroy" [("screen_name", sname)]
    return $ (const ()) <$> ret


-- | retweet status
retweet :: Token
        -> Integer -- ^ status id to retweet
        -> IO (Result Status)
retweet tok stid = do
    ret <- callAPI POST tok ("statuses/retweet/" ++ show stid) []
    return $ ret >>= decodeStatus


-- | get some user's recent statuses
getUserTimeline :: Token
                -> String -- ^ screen_name
                -> Maybe Int -- ^ number of record to retrieve
                -> IO (Result Timeline)
getUserTimeline tok sname mnum = do
    json <- callAPI GET tok "statuses/user_timeline" $ catMaybes [Just ("screen_name", sname)
                                                                 ,Just ("include_entities", "1")
                                                                 , ("count", ) . show <$> mnum]
    return $ json >>= decodeTimeline


-- | get rate limit status
-- return value is tuple of
-- (remaining_hits, hourly_limit)
getRateLimit :: Token -> IO (Result (Int, Int))
getRateLimit tok = do
    json <- callAPI GET tok "account/rate_limit_status" []
    return $
        case json of
            Ok (JSObject obj) -> (, ) <$> valFromObj "remaining_hits" obj <*> valFromObj "hourly_limit" obj
            Ok j              -> Error $ "invalid JSValue: " ++ show j
            -- to convert (Result JSValue) to (Result (Int, Int)),
            -- extract error message and re-wrap with Error.
            Error e           -> Error e


-- | call twitter API
-- callAPI GET tok "statuses/home_timeline" []
callAPI :: Method -> Token
        -> String             -- ^ path component of target API
        -> [(String, String)] -- ^ query parameters
        -> IO (Result JSValue)
callAPI m tok path query = callAPI' tok req EnumHttpClient
  where
    req = mkAPIRequest m (path ++ ".json") query


callAPI' :: Token -> Request -> EnumHttpClient -> IO (Result JSValue)
callAPI' tok req client = (runOAuthM tok $ do
    putToken tok
    debug req
    resp <- signRq2 HMACSHA1 Nothing req >>= serviceRequest client
    --debug resp
    case status resp of
        200 -> return $ decode $ BU.toString $ rspPayload resp
        -- maybe connection error
        0   -> return $ Error "unknown error"
        _   -> return $ Error $ show (status resp) ++ " " ++ show (reason resp))
  `catch` (\e -> return $ Error $ show e)


mkAPIRequest :: Method -> String -> [(String, String)] -> Request
mkAPIRequest = mkAPIRequest' twitterAPIURL twitterAPIVersion

mkAPIRequest' :: String -- API URL
              -> Int    -- API Version
              -> Method -> String -> [(String, String)] -> Request
mkAPIRequest' url ver m path query = req
  where
    rawReq = fromJust $ parseURL url
    req = rawReq { method = m
                 , pathComps = "" : show ver : splitBy (== '/') path
                 , qString = fromList query `union` qString rawReq
                 }


--
-- | User Stream API
--

userStreamIter :: Token -> (Status -> IO a) -> IO (Result ())
userStreamIter tok f = callStreamAPI handler GET tok "user" []
  where
    handler (W.Status sc _) hs = do
        splitStreamE =$ do
            -- drop initial stream, friends list
            EL.head
            E.continue go
        return (Response sc hs "")
    go (E.Chunks css) = do
        forM_ css $ \cs -> case decode cs >>= decodeStatus of
            Ok st   -> liftIO (f st) >> return ()
            Error e -> debug e
        E.continue go
    go E.EOF        = E.yield () E.EOF


-- | call twitter API
-- callStreamAPI handler GET tok "user" []
callStreamAPI :: EnumResponseHandler
              -> Method -> Token
              -> String             -- ^ path component of target API
              -> [(String, String)] -- ^ query parameters
              -> IO (Result ())
callStreamAPI iter m tok path query = do
    resp <- callAPI' tok req $ EnumHttpClientWithIter $ \s hs -> do
        resp <- iter s hs
        -- to always return JSNull
        return $ resp { responseBody = "null" }

    debug resp

    case resp of
        Ok JSNull -> return $ Ok ()
        Error e   -> return $ Error e
  where
    req = mkStreamAPIRequest m (path ++ ".json") query


mkStreamAPIRequest :: Method -> String -> [(String, String)] -> Request
mkStreamAPIRequest = mkAPIRequest' twitterStreamAPIURL twitterStreamAPIVersion

splitStreamE :: Monad m => E.Enumeratee BSU.ByteString String m b
splitStreamE iter =
    EB.splitWhen (==13 {- '\r' -})
        =$ EL.map BSU.toString
        =$ EL.map removeInitialNewline
        =$ EL.filter (/="") iter
  where
    removeInitialNewline ('\n':cs) = cs
    removeInitialNewline cs        = cs

--
-- | Twitter OAuth handler
--

reqURL, accURL, authURL :: String
reqURL = "https://twitter.com/oauth/request_token"
accURL = "https://twitter.com/oauth/access_token"
authURL = "https://twitter.com/oauth/authorize"

toAuthURL :: Token -> String
toAuthURL = ((authURL ++ "?oauth_token=") ++)
          . findWithDefault ("oauth_token", "")
          . oauthParams

doOAuth :: Application
       -> (String -> IO String) -- ^ IO action to display auth URL and ask verifier
       -> IO Token
doOAuth app asker = runOAuthM (fromApplication app) $ do
    -- get request token
    signRq2 HMACSHA1 Nothing (fromJust . parseURL $ reqURL) >>= oauthRequest EnumHttpClient
    -- display authorization URL to user, get verifier from user
    askAuthorizationBy toAuthURL asker
    -- get access token
    signRq2 HMACSHA1 Nothing (fromJust . parseURL $ accURL) >>= oauthRequest EnumHttpClient
    getToken

askAuthorizationBy :: MonadIO m => (Token -> String) -> (String -> IO String) -> OAuthMonadT m ()
askAuthorizationBy toURL asker = do
    token <- getToken
    ans <- liftIO $ asker (toURL token)
    putToken $ injectOAuthVerifier ans token
