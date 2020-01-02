{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Database.SQLite.Simple (Connection)

import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import System.IO

import Fingerd (UserRow, insertUser)

prompt :: String -> IO String
prompt text = do
  putStrLn text
  putStr "> "
  hFlush stdout
  getLine

updateUserQuery :: String -> Query
updateUserQuery field =
  "UPDATE users SET " <> Query (T.pack field) <> " =? WHERE username=?"

type UpdateRow = (Text, Text, Text)

insertUserPrompt :: Connection -> IO ()
insertUserPrompt conn = do
  username <- prompt "Enter username:"
  shell <- prompt "Enter shell:"
  homeDirectory <- prompt "Enter home directory:"
  realName <- prompt "Enter real name:"
  phone <- prompt "Enter phone:"
  SQLite.execute
    conn
    insertUser
    (Null, username, shell, homeDirectory, realName, phone)

updateUserPrompt :: Connection -> IO ()
updateUserPrompt conn = do
  username <- prompt "Enter username:"
  field <- prompt "Enter field to update:"
  value <- prompt "Enter new value:"
  SQLite.execute conn (updateUserQuery field) (value, username)

main :: IO ()
main = do
  putStrLn "Connecting to DB..."
  conn <- SQLite.open "finger.db"
  option <- prompt "What would you like to do? (update/insert)"
  case option of
    "update" -> updateUserPrompt conn
    "insert" -> insertUserPrompt conn
    _ -> putStrLn "Please enter either \"update\" or \"insert\""
  SQLite.close conn
