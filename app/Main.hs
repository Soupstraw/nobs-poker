{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class

import Data.Text

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

type NoBSAPI = Raw
          :<|> "socket" :> WebSocket

nobsAPI :: Proxy NoBSAPI
nobsAPI = Proxy

serveIndex :: Server Raw
serveIndex = serveDirectoryFileServer "client/public"

serveRoom :: MonadIO m => Connection -> m ()
serveRoom conn =
  do
    liftIO $ withPingThread conn 10 (return ()) (return ())
    liftIO $ putStrLn "New connection!"
    return ()

nobsServer :: Server NoBSAPI
nobsServer = serveIndex
        :<|> serveRoom

app :: Application
app = serve nobsAPI nobsServer

main :: IO ()
main = 
  do
    putStrLn $ "Server started at port 8080"
    run 8080 app

