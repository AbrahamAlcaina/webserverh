{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Web.Scotty

app :: ScottyM ()
app = do
  helloWorld
  helloYou

helloWorld :: ScottyM ()
helloWorld = get "/" $ text "Hello Worl!"

helloYou :: ScottyM ()
helloYou =
  get "/:name" $ do
    name <- param "name"
    text $ "Hello " <> name <> "!"

main :: IO ()
main = do
  putStrLn "http://localhost:8080/"
  scotty 8080 app
