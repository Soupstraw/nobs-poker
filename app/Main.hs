{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class

import Data.Map
import qualified Data.Text.Lazy as T

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


instance WebSocketsData UserMessage where
  fromDataMessage (Text _ (Just msg)) = 
    case head ws of
      "/help"    -> undefined
      "/setname" -> undefined
      "/join"    -> undefined
      _          -> UserMessage $ Say msg
    where
      ws = T.words msg
  fromLazyByteString = undefined
  toLazyByteString = undefined

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

