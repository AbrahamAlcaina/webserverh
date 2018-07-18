{-# LANGUAGE OverloadedStrings #-}

module Main where

import Users      (usersRoute)
import Web.Scotty

app :: ScottyM ()
app = do
  helloWorld
  usersRoute

helloWorld :: ScottyM ()
helloWorld = get "/" $ text "Hello World!"

main :: IO ()
main = do
  putStrLn "http://localhost:8080/"
  scotty 8080 app
