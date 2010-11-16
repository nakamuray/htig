{-# LANGUAGE CPP, OverloadedStrings #-}
module HTIG.Utils
    ( sendWelcome
    , writeServerCommand
    , writeServerError
    , writeJoin
    , userSpecToNickName
    , (++.)
    , getUserCacheDir
    , makeUserCacheDir
    , whenM
    , joinTo
    , isJoined
    , createChannelForName
    , lookupChannelFactory
    , updateTwitterStatus
    , withTransaction
    , withTransactionH
    , withConnection
    , withConnectionH
    , writeStatus
    , writeNoticeStatus
    , writeStatus'
    , b
    , s
    , snip
    ) where

import Control.Applicative ((<$>), (<*))
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Map as Map
import qualified Data.Set as Set

import HTIG.Core
import HTIG.Database hiding (Connection)
import HTIG.IRCServer
import HTIG.IRCServer.Utils
import HTIG.TwitterAPI

import qualified HTIG.Database (Connection)

#include "../debug.hs"


sendWelcome :: HTIG ()
sendWelcome = do
    sname <- gsServerName <$> getGlobal
    Just nick <- sNick <$> getLocal
    Just user <- sUserName <$> getLocal
    Just hname <- sHostName <$> getLocal
    let fullname = nick ++. "!" ++. user ++. "@" ++. hname
    writeServerCommand $ NumericCmd rPL_WELCOME
                         [nick, "Welcome to " ++. sname  ++." server " ++. fullname]

    writeServerCommand $ NumericCmd rPL_YOURHOST
                         [nick, "Your host is " ++. sname ++. ", running " ++. appName ++. "-" ++. appVersion]

    cd <- gsServerCreated <$> getGlobal
    writeServerCommand $ NumericCmd rPL_CREATED
                         [nick, "This server was created on " ++. (B.pack $ show cd)]

writeServerCommand :: Command -> HTIG ()
writeServerCommand cmd = do
    sname <- gsServerName <$> getGlobal
    writeConn' $ Message (Just $ Nick sname) cmd

writeServerError :: Integer -> B.ByteString -> HTIG ()
writeServerError num msg = writeServerCommand $ NumericCmd num $ [msg]

writeJoin :: UserSpec -> ChannelName -> HTIG ()
writeJoin u cname = writeConn' $ Message (Just u) $ JoinCmd $ Map.fromList [(cname, Nothing)]

userSpecToNickName :: UserSpec -> NickName
userSpecToNickName (Nick n)     = n
userSpecToNickName (User n _ _) = n

(++.) :: B.ByteString -> B.ByteString -> B.ByteString
(++.) = B.append

getUserCacheDir :: HTIG String
getUserCacheDir = do
    Just nick <- sNick <$> getLocal
    cpath <- getHTIGDir
    return $ cpath </> "cache" </> B.unpack nick

makeUserCacheDir :: HTIG ()
makeUserCacheDir = getUserCacheDir >>= liftIO . createDirectoryIfMissing True

whenM :: Monad m => m Bool -> m () -> m ()
whenM mBool m = mBool >>= \b -> when b m

joinTo :: ChannelName -> HTIG ()
joinTo cname = whenM (not <$> isJoined cname) $ do
    Just nick <- sNick <$> getLocal
    mchan <- createChannelForName cname
    case mchan of
        Just chan -> joinChannel chan (Nick nick)
        Nothing   -> return () -- XXX: should I return some error to client?

isJoined :: ChannelName -> HTIG Bool
isJoined cname = any ((cname ==) . channelName) . sJoinedChannels <$> getLocal

createChannelForName :: ChannelName -> HTIG (Maybe HChannel)
createChannelForName cname = do
    mcf <- lookupChannelFactory cname
    case mcf of
        Just cf -> do
            chan <- cf cname
            modifyLocal $ \s -> s { sJoinedChannels = chan : sJoinedChannels s }
            return $ Just chan
        Nothing -> return Nothing

lookupChannelFactory :: ChannelName -> HTIG (Maybe HChannelFactory)
lookupChannelFactory cname = do
    cfs <- channels . gsHConfig <$> getGlobal
    case filter (\(f, _) -> f cname) cfs of
        (_, cf):[] -> return $ Just cf
        []         -> return Nothing


updateTwitterStatus :: ChannelName -> CommandArg -> HTIG ()
updateTwitterStatus cname arg = do
    Just tok <- sToken <$> getLocal
    resSt <- liftIO $ updateStatus tok Nothing $ BU.toString arg
    case resSt of
        Ok _ -> do
            Just nick <- sNick <$> getLocal
            writeServerCommand $ NumericCmd rPL_TOPIC $ [nick, cname, arg]
        Error e ->
            writeServerCommand $ NoticeCmd (Set.singleton cname) $ BU.fromString $ "cannot update status: " ++ e
    return ()

withTransaction :: (HTIG.Database.Connection -> IO a) -> HTIG a
withTransaction h = withConnection h <* withConnection commit

withTransactionH :: (HTIG.Database.Connection -> HTIG a) -> HTIG a
withTransactionH h = withConnectionH h <* withConnection commit

withConnection :: (HTIG.Database.Connection -> IO a) -> HTIG a
withConnection h = withConnectionH $ liftIO . h

withConnectionH :: (HTIG.Database.Connection -> HTIG a) -> HTIG a
withConnectionH h = sDBConn <$> getLocal >>= h

writeStatus :: ChannelName -> Status -> HTIG ()
writeStatus = writeStatus' writePrivMsg' PrivMsg

writeNoticeStatus :: ChannelName -> Status -> HTIG ()
writeNoticeStatus = writeStatus' writeNotice' NoticeMsg

writeStatus' :: (UserSpec -> ChannelName -> CommandArg -> HTIG ())
             -> MessageType
             -> ChannelName -> Status -> HTIG ()
writeStatus' w t cname st = do
    hook <- statusHook . gsHConfig <$> getGlobal
    mst <- runHook hook (MsgCtx t cname) st
    case mst of
        Just st' -> let nick = BU.fromString $ usScreenName $ stUser st'
                    in w (Nick nick) cname $ BU.fromString $ stText st'
        Nothing  -> debug ("this status is dropped", st) >> return ()

b :: String -> BU.ByteString
b = BU.fromString

s :: BU.ByteString -> String
s = BU.toString

snip :: Int -> String -> String
snip n cs | length cs > (n - 3) = take (n - 3) cs ++ "..."
          | otherwise           = cs
