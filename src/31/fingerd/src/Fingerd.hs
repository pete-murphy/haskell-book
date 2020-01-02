{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Fingerd where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (bind, close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

data User =
  User
    { userId :: Integer
    , username :: Text
    , shell :: Text
    , homeDirectory :: Text
    , realName :: Text
    , phone :: Text
    }
  deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers =
  [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> pure Nothing
    [user] -> pure (Just user)
    _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  _ <- execute_ conn createUsers
  _ <- execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where
    meRow :: UserRow
    meRow =
      (Null, "peter", "/bin/zsh", "/Users/peter", "Pete Murphy", "800-468-3824")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat (intersperse "\n" usernames)
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
  BS.concat
    [ "Login: "
    , e username
    , "\t\t\t\t"
    , "Name: "
    , e realName
    , "\n"
    , "Directory: "
    , e homeDir
    , "\t\t\t\t"
    , "Shell: "
    , e shell
    , "\n"
    ]
  where
    e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn ("Couldn't find matching user for username: " <> show username)
      sendAll
        soc
        (BSC.pack $
         "Couldn't find matching user for username: " <>
         T.unpack (T.strip username))
    Just user -> sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

handleInsert :: Connection -> Socket -> IO ()
handleInsert dbConn soc = do
  msg <- recv soc 1024
  case T.words (decodeUtf8 msg) of
    username:realName:_ -> execute dbConn insertUser ((Null, username, "/bin/zsh", "/Users/" <> username, realName, "800-54-GIANT") :: UserRow)
    _ -> putStrLn "No good"


handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock =
  forever do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    handleQuery dbConn soc
    close soc

handleInserts :: Connection -> Socket -> IO ()
handleInserts dbConn sock =
  forever do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling insert"
    handleInsert dbConn soc
    close soc

main :: IO ()
main =
  withSocketsDo do
    serveraddr1:_ <-
      getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing
        (Just "79")
    serveraddr2:_ <-
      getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing
        (Just "80")
    sock1 <- socket (addrFamily serveraddr1) Stream defaultProtocol
    bind sock1 (addrAddress serveraddr1)
    listen sock1 1
    sock2 <- socket (addrFamily serveraddr2) Stream defaultProtocol
    bind sock2 (addrAddress serveraddr2)
    listen sock2 1
    conn <- open "finger.db"
    _ <- forkIO do handleQueries conn sock1
    handleInserts conn sock2
    SQLite.close conn
    close sock1
