{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module HTIG.Channel.OAuth
    ( oauthChannel
    ) where

import Control.Applicative ((<$>))
import Data.Binary (encodeFile)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Set as Set

import HTIG.Core
import HTIG.IRCServer
import HTIG.TwitterAPI
import HTIG.Utils


data OAuthChannel = OAuthChannel

oauthChannel :: HChannelFactory
oauthChannel = return . Channel OAuthChannel

instance IChannel OAuthChannel GlobalState SessionState where
    onJoin _ cname u = do
        let n = userSpecToNickName u
            nick = Nick n
        writeJoin nick cname
        st <- ask
        let asker url = runIRCM' (return url >>= h) st
            h url = do
                writePrivMsg' (Nick "oauth") cname $ "please visit this URL: " ++. B.pack url
                writePrivMsg' (Nick "oauth") cname $ "and post displayed text here"
                -- FIXME: should not use readConn directly in Channel class
                conn <- getConn
                msg <- liftIO $ readConn conn
                case msg of
                    Just (Message _ (PrivMsgCmd chans arg)) | "#oauth" `Set.member` chans -> return $ BU.toString arg
                    Just _ -> h url
                    -- TODO: error handling
                    Nothing -> return ""
        tok <- liftIO $ doOAuth htigApp asker
        modifyLocal $ \s -> s { sToken = Just tok }
        saveTwitterToken tok

saveTwitterToken :: Token -> HTIG ()
saveTwitterToken tok = do
    makeUserCacheDir
    cpath <- getUserCacheDir
    let tpath = cpath </> "token"
    liftIO $ encodeFile tpath tok
