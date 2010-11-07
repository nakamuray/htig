{-# LANGUAGE CPP, OverloadedStrings #-}
module HTIG.IRCServer.Connection
    ( Error
    , Connection
    , connAddr
    , acceptConn
    , socketToConn
    , closeConn
    , readConn
    , writeConn
    , isClosedConn
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Network.FastIRC.IO (hGetMessage)
import Network.FastIRC.Messages (Message, showMessage)
import Network.Socket (Socket, SockAddr, accept, socketToHandle)
import System.IO (Handle, IOMode(ReadWriteMode), BufferMode(NoBuffering), hClose, hIsClosed, hSetBuffering, hFlush)
import System.Mem.Weak (addFinalizer)

import qualified Data.ByteString.Char8 as B

#include "../../debug.hs"

type Error = String

type ReadChan = TChan (Maybe Message)
type WriteChan = TChan Message

data Connection = Conn { inChan :: ReadChan
                       , readerThreadId :: ThreadId
                       , outChan :: WriteChan
                       , writerThreadId :: ThreadId
                       , connHandle :: Handle
                       , connAddr :: SockAddr
                       }

instance Show Connection where
    show (Conn _ rid _ wid h addr) = "Conn { readerThreadId = " ++ show rid
                                       ++ ", writerThreadId = " ++ show wid
                                       ++ ", connHandle = " ++ show h
                                       ++ ", connAddr = " ++ show addr
                                       ++ " }"

acceptConn :: Socket -> IO Connection
acceptConn sock = accept sock >>= socketToConn

socketToConn :: (Socket, SockAddr) -> IO Connection
socketToConn (sock, addr) = do
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    (rid, ichan) <- startReaderThread h
    (wid, ochan) <- startWriterThread h
    let conn = Conn ichan rid ochan wid h addr
    addFinalizer conn $ do
        debug ("close connection in finalizer" :: String, rid, wid, h)
        killThread rid
        killThread wid
        hClose h
    return conn

startReaderThread :: Handle -> IO (ThreadId, ReadChan)
startReaderThread h = starter reader h

startWriterThread :: Handle -> IO (ThreadId, WriteChan)
startWriterThread h = starter writer h


starter :: (Handle -> TChan a -> IO ()) -> Handle -> IO (ThreadId, TChan a)
starter f h = do
    chan <- newTChanIO
    tid <- forkIO $ f h chan
    return (tid, chan)

reader :: Handle -> ReadChan -> IO ()
reader h chan = do
    mmsg <- (Just <$> hGetMessage h) `catch` (\e -> debug e >> return Nothing)
    case mmsg of
        Just msg -> do
            debug msg
            atomically $ writeTChan chan (Just msg)
            reader h chan
        Nothing  ->
            atomically $ writeTChan chan Nothing

writer :: Handle -> WriteChan -> IO ()
writer h chan = do
    msg <- atomically $ readTChan chan
    debug msg
    B.hPutStr h $ B.append (stripNewLine $ showMessage msg) "\r\n"
    hFlush h
    writer h chan

stripNewLine :: B.ByteString -> B.ByteString
stripNewLine bs | bs == B.empty = bs
                | otherwise     = let (h, t) = B.break isNewLine bs
                                  in h `B.append` " " `B.append` (stripNewLine $ B.dropWhile isNewLine t)

isNewLine :: Char -> Bool
isNewLine '\r' = True
isNewLine '\n' = True
isNewLine _    = False


closeConn :: Connection -> IO ()
closeConn conn = do
    debug ("closeConnection" :: String, conn)
    killThread $ readerThreadId conn
    killThread $ writerThreadId conn
    hClose $ connHandle conn

readConn :: Connection -> IO (Maybe Message)
readConn conn = atomically $ readTChan (inChan conn)

writeConn :: Connection -> Message -> IO ()
writeConn conn msg = atomically $ writeTChan (outChan conn) msg

isClosedConn :: Connection -> IO Bool
isClosedConn conn = hIsClosed $ connHandle conn
