module HTIG.Database
    ( openDatabase
    , getTLLastStatusId
    , getMentionLastStatusId
    , addToTimeline
    , addToMention
    , getStatusFromId

    , Connection

    -- re-export
    , commit
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Database.HDBC (run, runRaw, commit, quickQuery', fromSql, toSql)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)

import HTIG.TwitterAPI (Status(..), TwitterUser(..))

databaseSchema :: String
databaseSchema = unlines
    -- TODO: CREATE INDEX
    [ "CREATE TABLE IF NOT EXISTS user ("
    , "  id INTEGER NOT NULL PRIMARY KEY,"
    , "  name VARCHAR(255) NOT NULL,"
    --, "  screen_name VARCHAR(255) NOT NULL UNIQUE"
    , "  screen_name VARCHAR(255) NOT NULL"
    , ");"
    , "CREATE TABLE IF NOT EXISTS status ("
    , "  id INTEGER NOT NULL UNIQUE,"
    , "  created_at INTEGER NOT NULL,"
    , "  text VARCHAR(140) NOT NULL,"
    , "  source TEXT NOT NULL,"
    , "  truncated BOOL NOT NULL,"
    , "  in_reply_to_status_id INTEGER,"
    , "  in_reply_to_user_id INTEGER,"
    , "  favorited BOOL NOT NULL,"
    , "  in_reply_to_screen_name,"
    , "  user_id INTEGER NOT NULL,"
    , "  retweeted_status_id INTEGER REFERENCES status (id)"
    , ");"
    , "CREATE TABLE IF NOT EXISTS timeline ("
    , "  status_id INTEGER NOT NULL UNIQUE REFERENCES status (id)"
    , ");"
    , "CREATE TABLE IF NOT EXISTS mention ("
    , "  status_id INTEGER NOT NULL UNIQUE REFERENCES status (id)"
    , ");"
    ]


openDatabase :: FilePath -> IO Connection
openDatabase fpath = do
    conn <- connectSqlite3 fpath
    runRaw conn databaseSchema
    commit conn
    return conn


getTLLastStatusId :: String -> Connection -> IO (Maybe Integer)
getTLLastStatusId = getLastStatusId "timeline"

getMentionLastStatusId :: String -> Connection -> IO (Maybe Integer)
getMentionLastStatusId = getLastStatusId "mention"

getLastStatusId :: String -> String -> Connection -> IO (Maybe Integer)
getLastStatusId tbl sname conn = do
    result <- quickQuery' conn ("SELECT status_id FROM " ++ tbl ++ " \
                                \ ORDER BY status_id DESC LIMIT 1")
                                []
    case result of
        [val]:_ -> return $ Just $ fromSql val
        []      -> return Nothing
        x       -> fail $ "unexpexted result: " ++ show x


-- TODO: addToTimeline, addToMention をうまいことまとめる
addToTimeline :: String -> Status -> Connection -> IO ()
addToTimeline sname st conn = do
    addStatusIfNotExists st conn
    run conn "INSERT INTO timeline (status_id) VALUES (?)" [toSql $ stId st]
    return ()


addToMention :: String -> Status -> Connection -> IO ()
addToMention sname st conn = do
    addStatusIfNotExists st conn
    run conn "INSERT INTO mention (status_id) VALUES (?)" [toSql $ stId st]
    return ()


addStatusIfNotExists :: Status -> Connection -> IO ()
addStatusIfNotExists st conn = do
    [val]:_ <- quickQuery' conn "SELECT COUNT(*) FROM user where id = ?" [toSql $ usId . stUser $ st]
    when ((fromSql val :: Int) == 0) $ do
        run conn "INSERT INTO user (id, name, screen_name) VALUES (?, ?, ?)"
                 [ toSql $ usId . stUser $ st
                 , toSql $ usName . stUser $ st
                 , toSql $ usScreenName . stUser $ st
                 ]
        return ()

    when (isJust $ stRetweetedStatus st) $
        addStatusIfNotExists (fromJust $ stRetweetedStatus st) conn

    [val']:_ <- quickQuery' conn "SELECT COUNT(*) FROM status WHERE id = ?" [toSql $ stId st]
    when ((fromSql val' :: Int) == 0) $ do
        run conn "INSERT INTO status (id, created_at, text, source, truncated, in_reply_to_status_id, \
                                    \ in_reply_to_user_id, favorited, in_reply_to_screen_name, user_id, retweeted_status_id) \
                 \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 [ toSql $ stId st
                 , toSql $ stCreatedAt st
                 , toSql $ stText st
                 , toSql $ stSource st
                 , toSql $ stTruncated st
                 , toSql $ stInReplyToStatusId st
                 , toSql $ stInReplyToUserId st
                 , toSql $ stFavorited st
                 , toSql $ stInReplyToScreenName st
                 , toSql $ (usId . stUser) st
                 , toSql $ stId <$> stRetweetedStatus st
                 ]
        return ()

getStatusFromId :: Integer -> Connection -> IO (Maybe Status)
getStatusFromId stid conn = do
    result <- quickQuery' conn "SELECT status.id, created_at, text, source, truncated, in_reply_to_status_id, in_reply_to_user_id, \
                                     \ favorited, in_reply_to_screen_name, user.id, user.name, user.screen_name, retweeted_status_id \
                                \ FROM status LEFT JOIN user ON (status.user_id = user.id) \
                                \ WHERE status.id = ? LIMIT 1" [toSql stid]
    case result of
        [] -> return Nothing
        [vId, vCreatedAt, vText, vSource, vTruncated, vInReplyToStatusId, vInReplyToUserId,
         vFavorited, vInReplyToScreenName, vUserId, vUserName, vUserScreenName, vRetweetedStatusId]:_ -> do
            retweetedStatus <-
                case fromSql vRetweetedStatusId of
                    Just rsid -> getStatusFromId rsid conn
                    Nothing   -> return Nothing
            return $ Just $ Status (fromSql vCreatedAt)
                                   (fromSql vId)
                                   (fromSql vText)
                                   (fromSql vSource)
                                   (fromSql vTruncated)
                                   (fromSql vInReplyToStatusId)
                                   (fromSql vInReplyToUserId)
                                   (fromSql vFavorited)
                                   (fromSql vInReplyToScreenName)
                                   (TwitterUser (fromSql vUserId) (fromSql vUserName) (fromSql vUserScreenName))
                                   retweetedStatus
        x -> fail $ "unexpexted result: " ++ show x

-- TODO: export these functions
garbageCollect :: Connection -> IO ()
garbageCollect conn = do
    clearOldTimeline conn
    clearOldMentions conn
    clearUnreferencedStatuses conn

clearOldTimeline :: Connection -> IO ()
clearOldTimeline conn = do
    c <- run conn "DELETE FROM timeline WHERE status_id <= (SELECT status_id FROM timeline ORDER BY status_id LIMIT 1 OFFSET 100)" []
    print ("delete", c, "old timeline") -- debug print

clearOldMentions :: Connection -> IO ()
clearOldMentions conn = do
    c <- run conn "DELETE FROM mention WHERE status_id <= (SELECT status_id FROM mention ORDER BY status_id LIMIT 1 OFFSET 100)" []
    print ("delete", c, "old mentions") -- debug print

clearUnreferencedStatuses :: Connection -> IO ()
clearUnreferencedStatuses conn = do
    c <- run conn "DELETE FROM status \
                  \ WHERE (SELECT COUNT(*) FROM timeline WHERE status_id = id) = 0 \
                  \   AND (SELECT COUNT(*) FROM mention WHERE status_id = id) = 0 \
                  \   AND (SELECT COUNT(*) FROM status as s WHERE s.retweeted_status_id = status.id) = 0" []
    print ("delete", c, "unreferenced statuses") -- debug print
