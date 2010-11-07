{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings #-}
module HTIG.Core
    ( appName
    , appVersion
    , htigApp
    , GlobalState(..)
    , SessionState(..)
    , newEmptyState
    , HTIG
    , HChannel
    , HChannelFactory
    , Action
    , StatusHook
    , MessageType(..)
    , MessageContext(..)
    , Query(..)
    , runQuery
    , runHook
    , HConfig(..)
    , getHTIGDir
    , forkH
    , killH
    , killAllH

    , module Control.Monad.Reader
    , module Control.Monad.Trans
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT(runReaderT), liftM2, MonadReader, MonadIO, ask, asks)
import Control.Monad.Trans
import Data.List (delete)
import Data.Monoid (Monoid(mempty, mappend), Endo(appEndo))
import Data.Time (UTCTime)
import Network.OAuth.Consumer (Application(Application), OAuthCallback(OOB))
import System.Directory (getAppUserDataDirectory)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified HTIG.Database as DB
import HTIG.IRCServer
import HTIG.TwitterAPI

import Paths_htig (version)
import Data.Version (showVersion)

#include "../debug.hs"


appName :: B.ByteString
appName = "htig"

appVersion :: B.ByteString
appVersion = B.pack $ showVersion version

-- | twitter oauth application
htigApp :: Application
htigApp = Application "HQw2EU5rwjzBPsP8DnpOZA" "4u2sHLcpSfyo4u4iLm945lv0irBU12YEPXVqPfPNSsY" OOB


data GlobalState = GS
    { gsServerName    :: B.ByteString
    , gsServerCreated :: UTCTime
    , gsHConfig       :: HConfig
    , gsNicks         :: [NickName]
    }

data SessionState = SS
    { sNick :: Maybe NickName
    , sPassword :: Maybe CommandArg
    , sUserName :: Maybe NickName
    , sHostName :: Maybe CommandArg
    , sServerName :: Maybe CommandArg
    , sRealName :: Maybe CommandArg
    , sToken :: Maybe Token
    , sThreadIds :: [ThreadId]
    , sDBConn :: DB.Connection
    , sJoinedChannels :: [HChannel]
    }

newEmptyState :: SessionState
newEmptyState = SS
    { sNick = Nothing
    , sPassword = Nothing
    , sUserName = Nothing
    , sHostName = Nothing
    , sServerName = Nothing
    , sRealName = Nothing
    , sToken = Nothing
    , sThreadIds = []
    , sDBConn = undefined
    , sJoinedChannels = []
    }

type HTIG = IRCM GlobalState SessionState
type HChannel = Channel GlobalState SessionState
type HChannelFactory = ChannelName -> HTIG HChannel

type Action = Set.Set TargetName -> B.ByteString -> HTIG ()
type StatusHook = Query (Endo (HTIG (Maybe Status)))

data MessageType = PrivMsg | NoticeMsg
    deriving (Eq)

data MessageContext = MsgCtx
    { cMsgType :: MessageType
    , cChan    :: ChannelName
    }

newtype Query a = Query (ReaderT MessageContext HTIG a)
    deriving (Functor, Monad, MonadReader MessageContext, MonadIO)

runQuery :: Query a -> MessageContext -> HTIG a
runQuery (Query m) c = runReaderT m c

runHook :: StatusHook -> MessageContext -> Status -> HTIG (Maybe Status)
runHook h ctx st = do
    f <- runQuery h ctx
    appEndo f (return $ Just st)

instance Monoid a => Monoid (Query a) where
    mempty = return mempty
    mappend = liftM2 mappend

data HConfig = HConfig
    { listenAddr      :: String
    , listenPort      :: Int
    , sessionInitHook :: HTIG ()
    , channels        :: [(ChannelName -> Bool, HChannelFactory)]
    , actions         :: [(B.ByteString, Action)]
    , statusHook      :: StatusHook
    , errorMessage    :: Maybe String
    }

getHTIGDir :: MonadIO m => m FilePath
getHTIGDir = liftIO $ getAppUserDataDirectory $ B.unpack appName

forkH :: HTIG () -> HTIG ThreadId
forkH h = do
    st <- ask
    i <- liftIO $ forkIO $ runIRCM' h st
    modifyLocal $ \s -> s { sThreadIds = i : sThreadIds s }
    return i

killH :: ThreadId -> HTIG ()
killH i = do
    liftIO $ killThread i
    modifyLocal $ \s -> s { sThreadIds = delete i $ sThreadIds s }

killAllH :: HTIG ()
killAllH = sThreadIds <$> getLocal >>= mapM_ killH
