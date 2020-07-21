{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class

import Data.Text

import Html.Index

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket
import Servant.HTML.Blaze

import Text.Blaze.Html5 hiding (main)

type NoBSAPI = Raw

nobsAPI :: Proxy NoBSAPI
nobsAPI = Proxy

serveIndex :: Server Raw
serveIndex = serveDirectoryFileServer "public"

nobsServer :: Server NoBSAPI
nobsServer = serveIndex

app :: Application
app = serve nobsAPI nobsServer

main :: IO ()
main = 
  do
    putStrLn $ "Server started at port 8080"
    run 8080 app

