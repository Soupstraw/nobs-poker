{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class

import Data.Map
import Data.Text

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

type NoBSAPI = "socket" :> WebSocket
          :<|> Raw

type UserID = Int
type RoomID = Int

data User = User
  { _userName :: Text
  , _userConn :: Connection
  }

data Room = Room
  { _roomUsers :: [UserID]
  }

data ServerState = ServerState
  { _ssUserPool :: Map UserID User
  , _ssRoomPool :: Map RoomID Room
  }

data UserMessage = UserMessage
  { _umAction :: UserAction
  , _umRoom   :: RoomID
  }

data UserAction 
  = SetName Text
  | JoinRoom RoomID
  | Say Text

nobsAPI :: Proxy NoBSAPI
nobsAPI = Proxy

serveIndex :: Server Raw
serveIndex = serveDirectoryFileServer "client/public"

serveSocket 
  :: MonadIO m 
  => Connection 
  -> m ()
serveSocket conn =
  do
    liftIO $ do
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
    serveClient conn

nobsServer :: Server NoBSAPI
nobsServer = serveSocket
        :<|> serveIndex

app :: Application
app = serve nobsAPI nobsServer

main :: IO ()
main = 
  do
    putStrLn $ "Server started at port 8080"
    run 8080 app

