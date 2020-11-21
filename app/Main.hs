{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Relude
import Relude.Extra.Map

import Control.Monad.Random
import Control.Lens hiding (Fold)

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

import CommandParser

type UserID = Word
type RoomID = Word

data User = User
  { _userName :: MVar Text
  , _userConn :: MVar Connection
  }
makeLenses 'User

data Room = Room
  { _roomUsers :: MVar [UserID]
  }

data ServerState = ServerState
  { _ssUserPool :: Map UserID User
  , _ssRoomPool :: Map RoomID Room
  }
makeLenses 'ServerState

type NoBSAPI = "socket" :> WebSocket
          :<|> "room" :> Capture "roomId" Text :> Raw
          :<|> Raw
type NoBSM = ReaderT (IORef ServerState) (RandT StdGen Handler)

emptyState :: ServerState
emptyState = ServerState mempty mempty

runNoBSM :: NoBSM a -> IORef ServerState -> Handler a
runNoBSM m s = evalRandT (runReaderT m s) (mkStdGen 0)

runNoBSM_ :: NoBSM a -> IORef ServerState -> IO a
runNoBSM_ m st = 
  do
    res <- runHandler $ evalRandT (runReaderT m st) (mkStdGen 0)
    case res of
      Right x -> return x
      Left x  -> error $ "Server error: " <> show x

addUser 
  :: ( MonadRandom m
     , MonadReader (IORef ServerState) m
     , MonadIO m
     )
  => Connection 
  -> m UserID
addUser conn = 
  do
    ior <- ask
    st <- liftIO $ readIORef ior
    let userPool = st ^. ssUserPool
    uid <- getRandom
    if member uid userPool
      then addUser conn
      else do
        let pname = "Player#" <> show uid
        mvarConn <- newMVar conn
        mvarName <- newMVar pname
        let newUser = User mvarName mvarConn
        modifyIORef ior $ ssUserPool %~ insert uid newUser
        putTextLn $ "Added player " <> pname
        return uid

nobsAPI :: Proxy NoBSAPI
nobsAPI = Proxy

serveIndex :: ServerT Raw m
serveIndex = serveDirectoryFileServer "client"

serveSocket 
  :: ( MonadIO m
     , MonadReader (IORef ServerState) m
     , MonadRandom m
     )
  => Connection 
  -> m ()
serveSocket conn =
  do
    putStrLn "New connection!"
    liftIO $ sendTextData conn ("Welcome!" :: Text)
    uid <- addUser conn
    serveClient uid

serveClient 
  :: ( MonadIO m
     , MonadRandom m
     , MonadReader (IORef ServerState) m
     )
  => UserID 
  -> m ()
serveClient uid =
  do
    ior <- ask
    st <- liftIO $ readIORef ior
    let mconn = st ^? ssUserPool . at uid . _Just . userConn
    case mconn of
      Just x -> do
        conn <- takeMVar x
        msg <- liftIO $ receiveData conn
        putMVar x conn
        liftIO $ handleMsg uid msg
        serveClient uid
      Nothing -> putStrLn $ "Failed to get connection by uid " <> show uid

handleMsg 
  :: MonadIO m
  => UserID
  -> Text
  -> m ()
handleMsg userId msg = 
  do
    putTextLn $ "Parsing message from " <> show userId
    putTextLn $ show msg
    let (roomId, command) = parseMessage msg
    putTextLn $ "Message sent to room " <> show roomId
    case command of
      Join x  -> putTextLn $ "join "  <> show x
      Say x   -> putTextLn $ show x
      Raise x -> putTextLn $ "raise " <> show x
      Call    -> putTextLn "call"
      Fold    -> putTextLn "fold"
      Error x -> putTextLn $ "command parse error: " <> x

nobsServer 
  :: ( MonadIO m
     , MonadReader (IORef ServerState) m
     , MonadRandom m
     )
  => ServerT NoBSAPI m
nobsServer = serveSocket
        :<|> const serveIndex
        :<|> serveIndex

app :: IORef ServerState -> Application
app s = serve nobsAPI $ hoistServer nobsAPI (`runNoBSM` s) nobsServer

main :: IO ()
main = 
  do
    putStrLn "Starting server at port 8080"
    ref <- newIORef emptyState
    run 8080 (app ref)

