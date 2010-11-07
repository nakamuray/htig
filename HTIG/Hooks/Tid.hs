module HTIG.Hooks.Tid
    ( doShowTid

    , createTidTable

    , Tid
    , getTidFromStatus
    , getStatusFromTid

    , vowel
    , consonant
    , tidTable
    , nextTid
    ) where

import Database.HDBC (run, quickQuery', fromSql, toSql)

import HTIG.Core
import HTIG.Database
import HTIG.StatusHook
import HTIG.TwitterAPI
import HTIG.Utils

doShowTid :: StatusHook
doShowTid = doH' $ \st -> do
    tid <- withTransaction $ \conn -> do
               t <- genTid conn
               saveTid st t conn
               return t
    let text = stText st ++ " \x03\&10[" ++ tid ++ "]\x0f"
    return $ Just $ st { stText = text }


type Tid = String

tidTableSchema :: String
tidTableSchema = unlines
    [ "CREATE TABLE IF NOT EXISTS tid ("
    , "  id INTEGER NOT NULL PRIMARY KEY,"
    , "  status_id INTEGER NOT NULL REFERENCES status (id),"
    , "  tid VARCHAR(8) NOT NULL"
    , ");"
    ]

createTidTable :: HTIG ()
createTidTable = withTransaction createTidTable'

createTidTable' :: Connection -> IO ()
createTidTable' conn = run conn tidTableSchema [] >> return ()

saveTid :: Status -> Tid -> Connection -> IO ()
saveTid st tid conn = run conn "INSERT INTO tid (status_id, tid) VALUES (?, ?)" [toSql $ stId st, toSql tid] >> return ()

genTid :: Connection -> IO Tid
genTid conn = do
    result <- quickQuery' conn "SELECT tid FROM tid ORDER BY id DESC LIMIT 1" []
    case result of
        [val]:_ -> return $ nextTid $ fromSql val
        []      -> return $ head tidTable
        x       -> fail $ "unexpexted result: " ++ show x

nextTid :: Tid -> Tid
nextTid tid = case break (== tid) tidTable of
                  (_, _:[])     -> head tidTable
                  (_, _:tid':_) -> tid'

vowel, consonant :: [String]
vowel = ["a", "i", "u", "e", "o"]
consonant = ["", "k", "s", "t", "n", "h", "m", "y", "r", "w"]

tidTable :: [Tid]
tidTable = do
    c1 <- consonant
    v1 <- vowel
    c2 <- consonant
    v2 <- vowel
    return (c1 ++ v1 ++ c2 ++ v2)

getTidFromStatus :: Status -> Connection -> IO (Maybe Tid)
getTidFromStatus st conn = do
    result <- quickQuery' conn "SELECT tid FROM tid WHERE status_id = ?" [toSql $ stId st]
    case result of
        [val]:_ -> return $ Just $ fromSql val
        []      -> return Nothing
        x       -> fail $ "unexpexted result: " ++ show x

getStatusFromTid :: Tid -> Connection -> IO (Maybe Status)
getStatusFromTid tid conn = do
    result <- quickQuery' conn "SELECT status_id FROM tid WHERE tid = ? ORDER BY id DESC LIMIT 1" [toSql tid]
    case result of
        [vId]:_ -> getStatusFromId (fromSql vId) conn
        []      -> return Nothing
        x       -> fail $ "unexpexted result: " ++ show x
