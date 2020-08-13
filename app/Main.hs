{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class

import Data.Map
import qualified Data.Text.Lazy as T

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

import CommandParser

type NoBSAPI = "socket" :> WebSocket
          :<|> Raw

type UserID = Int
type RoomID = Int

data User = User
  { _userName :: T.Text
  , _userConn :: Connection
  }

data Room = Room
  { _roomUsers :: [UserID]
  }

data ServerState = ServerState
  { _ssUserPool :: Map UserID User
  , _ssRoomPool :: Map RoomID Room
  }

nobsAPI :: Proxy NoBSAPI
nobsAPI = Proxy

serveIndex :: Server Raw
serveIndex = serveDirectoryFileServer "client/public"

serveSocket 
  :: MonadIO m 
  => Connection 
  -> m ()
serveSocket conn = liftIO $
  do
    putStrLn "New connection!"
    forkIO $ serveClient conn
    return ()

serveClient 
  :: MonadIO m 
  => Connection 
  -> m ()
serveClient conn =
  do
    msg <- liftIO $ receiveData conn
    let act = msg ^. umAction
    let notImplementedMsg :: T.Text
        notImplementedMsg = "Command not implemented"
    liftIO $ case act of
      Say x        -> sendTextData conn x
      ParseError x -> sendTextData conn x
      _            -> sendTextData conn notImplementedMsg
    serveClient conn

nobsServer :: Server NoBSAPI
nobsServer = serveSocket
        :<|> serveIndex

app :: Application
app = serve nobsAPI nobsServer

main :: IO ()
main = 
  do
    putStrLn "Server started at port 8080"
    run 8080 app

