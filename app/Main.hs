{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Relude hiding (ByteString)
import Relude.Extra.Map

import Control.Concurrent.MVar (withMVar)
import Control.Monad.Random
import Control.Lens hiding (Fold)

import Data.Aeson
import Data.ByteString.Lazy (ByteString)

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

import Shared

newtype UserID = UserID Text
  deriving (Eq, Ord, Show)

instance Random UserID where
  random g =
    do
      let (n, g') = next g
      (UserID $ show n, g')

type RoomID = Text

data User = User
  { _userName :: MVar Text
  , _userConn :: MVar Connection
  , _userRoom :: MVar (Maybe RoomID)
  }
makeLenses 'User

data Room = Room
  { _roomUsers :: MVar [UserID]
  }
makeLenses 'Room

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

addUser 
  :: ( MonadRandom m
     , MonadReader (IORef ServerState) m
     , MonadIO m
     )
  => Connection 
  -> m UserID
addUser conn = 
  do
    st <- readIORef =<< ask
    let userPool = st ^. ssUserPool
    uid <- getRandom
    if member uid userPool
      then addUser conn
      else do
        let pname = "Player#" <> show uid
        mvarConn <- newMVar conn
        mvarName <- newMVar pname
        mvarRoom <- newEmptyMVar
        let newUser = User mvarName mvarConn mvarRoom
        mdf <- modifyIORef <$> ask
        mdf $ ssUserPool %~ insert uid newUser
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
    putTextLn "New connection!"
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
    st <- readIORef =<< ask
    let mconn = st ^? ssUserPool . at uid . _Just . userConn
    case mconn of
      Just x -> do
        conn <- takeMVar x
        msg <- liftIO $ receiveData conn
        putMVar x conn
        handleMsg uid msg
        serveClient uid
      Nothing -> putStrLn $ "Failed to get connection by uid " <> show uid

handleMsg 
  :: ( MonadIO m
     , MonadReader (IORef ServerState) m
     )
  => UserID
  -> ByteString
  -> m ()
handleMsg userId msg = 
  do
    putTextLn $ "Parsing message from " <> show userId
    case decode msg of
      Nothing  -> putTextLn $ "Failed to parse message: " <> show msg
      Just cmd -> doCommand userId cmd

doCommand 
  :: ( MonadIO m
     , MonadReader (IORef ServerState) m
     )
  => UserID 
  -> ClientMsg 
  -> m ()
doCommand userId (CJoin roomId) = 
  do
    st <- getServerState
    case st ^. ssRoomPool . at roomId of
      Just room -> 
        do
          void . liftIO . withMVar (room ^. roomUsers) $ \usrs ->
            if userId `elem` usrs
              then do
                putTextLn "Player is already in room"
                return usrs
              else do
                sendMessage userId $ SList
                return $ cons userId usrs
      Nothing   -> putTextLn $ "Room " <> show roomId <> " does not exist."
doCommand _ _ = undefined

sendMessage userId msg = 
  do
    undefined

getServerState
  :: ( MonadIO m
     , MonadReader (IORef ServerState) m
     )
  => m ServerState
getServerState = readIORef =<< ask

modifyServerState
  :: ( MonadIO m
     , MonadReader (IORef ServerState) m
     )
  => (ServerState -> ServerState) -> m ()
modifyServerState f = (`modifyIORef` f) =<< ask

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
    let modulePath = "client/src/NoBSAPI.elm" 
    putTextLn $ "Generating Elm API module at " <> toText modulePath
    writeFileText modulePath generateModule
    putTextLn "Starting server at port 8080"
    ref <- newIORef emptyState
    run 8080 (app ref)

