{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Relude
import Relude.Extra.Map

import Control.Monad.Random
import Control.Lens

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

type UserID = Int
type RoomID = Int

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
        liftIO . handleMsg uid $ toString (msg :: Text)
        serveClient uid
      Nothing -> putStrLn $ "Failed to get connection by uid " <> show uid

handleMsg 
  :: MonadIO m
  => UserID
  -> String 
  -> m ()
handleMsg uid "/call" = putStrLn $ show uid <> ": Call"
handleMsg uid "/fold" = putStrLn $ show uid <> ": Fold"
handleMsg uid msg
  | "/raise " `isPrefixOf` msg = putStrLn $ show uid <> ": Raise " <> drop 7 msg
  | "/" `isPrefixOf` msg = putStrLn $ "Unknown command: " <> msg
  | null msg = putStrLn "Empty message"
  | otherwise = putStrLn $ "Say " <> msg

nobsServer 
  :: ( MonadIO m
     , MonadReader (IORef ServerState) m
     , MonadRandom m
     )
  => ServerT NoBSAPI m
nobsServer = serveSocket
        :<|> serveIndex

app :: IORef ServerState -> Application
app s = serve nobsAPI $ hoistServer nobsAPI (flip runNoBSM s) nobsServer

main :: IO ()
main = 
  do
    putStrLn "Starting server at port 8080"
    ref <- newIORef emptyState
    run 8080 (app ref)

