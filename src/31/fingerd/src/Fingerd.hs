{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Fingerd where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, ReaderT(..), runReaderT)
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

type App = ReaderT Connection IO 

getUser :: Text -> App (Maybe User)
getUser username = do
  conn <- ask
  results <- liftIO $ query conn getUserQuery (Only username)
  case results of
    [] -> pure Nothing
    [user] -> pure (Just user)
    _ -> liftIO $ throwIO DuplicateData

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

returnUsers :: Socket -> App ()
returnUsers soc = do
  dbConn <- ask
  rows <- liftIO $ query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat (intersperse "\n" usernames)
  liftIO $ sendAll soc (encodeUtf8 newlineSeparated)

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

returnUser :: Socket -> Text -> App ()
returnUser soc username = do
  maybeUser <- getUser (T.strip username)
  liftIO $ case maybeUser of
    Nothing -> do
      putStrLn ("Couldn't find matching user for username: " <> show username)
      sendAll
        soc
        (BSC.pack $ 
         "Couldn't find matching user for username: " <>
         T.unpack (T.strip username))
    Just user -> sendAll soc (formatUser user)

handleQuery :: Socket -> App ()
handleQuery soc = do
  msg <- liftIO $ recv soc 1024
  case msg of
    "\r\n" -> returnUsers soc
    name -> returnUser soc (decodeUtf8 name)

handleInsert :: Socket -> App ()
handleInsert soc = do
  conn <- ask
  msg <- liftIO $ recv soc 1024
  case T.words (decodeUtf8 msg) of
    username:realName:_ -> liftIO $ 
      execute conn insertUser ((Null, username, "/bin/zsh", "/Users/" <> username, realName, "800-54-GIANT") :: UserRow)
    _ -> liftIO $ putStrLn "No good"


handleQueries :: Socket -> App ()
handleQueries sock = do
    (soc, _) <- liftIO (accept sock)
    liftIO $ putStrLn "Got connection, handling query"
    handleQuery soc
    liftIO (close soc)

handleInserts :: Socket -> App ()
handleInserts sock = do
    (soc, _) <- liftIO $ accept sock
    liftIO $ putStrLn "Got connection, handling insert"
    handleInsert soc
    liftIO (close soc)

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
    _ <- forkIO $ forever $ runReaderT (handleQueries sock1) conn
    forever $ runReaderT (handleInserts sock2) conn
    SQLite.close conn
    close sock1
