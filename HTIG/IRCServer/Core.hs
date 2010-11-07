{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HTIG.IRCServer.Core
    ( IRCState(..)

    , IRCM
    , runIRCM
    , runIRCM'

    , getConn
    , getGlobal
    , setGlobal
    , modifyGlobal
    , modifyGlobal'

    , getLocal
    , setLocal
    , modifyLocal
    , modifyLocal'

    , liftIO
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)

import HTIG.IRCServer.Connection (Connection)


data IRCState g l = IRCState { ircGlobal :: TVar g
                             , ircLocal  :: TVar l
                             , ircConn   :: Connection
                             }

newtype IRCM g l a = IRCM { unIRCM :: ReaderT (IRCState g l) IO a }
    deriving (Monad, Functor, MonadIO, MonadReader (IRCState g l))

instance Applicative (IRCM g l) where
    pure = return
    f <*> x = do
        f' <- f
        x' <- x
        return $ f' x'

runIRCM :: IRCM g l a -> TVar g -> TVar l -> Connection -> IO a
runIRCM m g l conn = runIRCM' m $ IRCState g l conn

runIRCM' :: IRCM g l a -> IRCState g l -> IO a
runIRCM' m s = runReaderT (unIRCM m) s


getConn :: IRCM g l Connection
getConn = asks ircConn

getGlobal :: IRCM g l g
getGlobal = mkGet ircGlobal

setGlobal :: g -> IRCM g l ()
setGlobal g = mkSet ircGlobal g

modifyGlobal :: (g -> g) -> IRCM g l ()
modifyGlobal f = mkModify ircGlobal $ \g -> (f g, ())

modifyGlobal' :: (g -> (g, a)) -> IRCM g l a
modifyGlobal' f = mkModify ircGlobal f

getLocal :: IRCM g l l
getLocal = mkGet ircLocal

setLocal :: l -> IRCM g l ()
setLocal l = mkSet ircLocal l

modifyLocal :: (l -> l) -> IRCM g l ()
modifyLocal f = mkModify ircLocal $ \l -> (f l, ())

modifyLocal' :: (l -> (l, a)) -> IRCM g l a
modifyLocal' f = mkModify ircLocal f


mkGet :: (IRCState g l -> TVar a) -> IRCM g l a
mkGet f = liftIO . atomically . readTVar =<< asks f

mkSet :: (IRCState g l -> TVar a) -> a -> IRCM g l ()
mkSet f v = liftIO . atomically . flip writeTVar v =<< asks f

mkModify :: (IRCState g l -> TVar a) -> (a -> (a, b)) -> IRCM g l b
mkModify f f' = do
    tv <- asks f
    liftIO $ atomically $ do
        v <- readTVar tv
        let (v', r) = f' v
        writeTVar tv v'
        return r
