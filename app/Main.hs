{-# LANGUAGE OverloadedStrings #-}

module Main where

import Users      (usersRoute)
import Web.Scotty

import Control.Monad.Logger    (NoLoggingT(..), runNoLoggingT)
import Data.Text               (pack)
import Database.Persist.Sqlite

import Eventful.Store.Sqlite

app :: ConnectionPool -> ScottyM ()
app pool = do
  usersRoute pool

main :: IO ()
main = do
  pool <- setup
  putStrLn "http://localhost:8080/"
  scotty 8080 $ app pool

setup :: IO ConnectionPool
setup =
    runNoLoggingT $ withSqlitePool "data.db" 10 $ \pool -> NoLoggingT $ do
    initializeSqliteEventStore defaultSqlEventStoreConfig pool
    return pool

