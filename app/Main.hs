{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Relude hiding (ByteString)
import Relude.Extra.Map

import Control.Exception (bracket)
import Control.Monad.Random
import Control.Lens hiding (Fold)

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Map ()

import Katip

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

import qualified Text.Show as T

import Shared

newtype Unique = Unique Text
  deriving (Eq, Ord)

instance Show Unique where
  show (Unique x) = toString x

instance Random Unique where
  random g = (Unique $ show (x :: Word64), g')
    where (x, g') = random g

data User = User
  { _userName :: MVar Text
  , _userConn :: MVar Connection
  , _userRoom :: MVar (Maybe Unique)
  }
makeLenses 'User

data Room = Room
  { _roomUsers :: MVar [Unique]
  }
makeLenses 'Room

data ServerState = ServerState
  { _ssUserPool :: Map Unique User
  , _ssRoomPool :: Map Unique Room
  }
makeLenses 'ServerState

type NoBSAPI = "socket" :> WebSocket
          :<|> "room" :> Capture "roomId" Text :> Raw
          :<|> Raw

data NoBSState = NoBSState
  { _nsServerState  :: IORef ServerState
  , _nsLogNamespace :: Namespace
  , _nsLogContext   :: LogContexts
  , _nsLogEnv       :: LogEnv
  }
makeLenses ' NoBSState

newtype NoBS a = NoBS
  { unNoBS :: ReaderT NoBSState (RandT StdGen (KatipContextT Handler)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader NoBSState
             , MonadRandom 
             , MonadIO
             )

instance Katip NoBS where
  getLogEnv = view nsLogEnv
  localLogEnv  f (NoBS m) = NoBS $ local (over nsLogEnv f) m

instance KatipContext NoBS where
  getKatipContext = view nsLogContext
  localKatipContext f (NoBS m) = NoBS $ local (over nsLogContext f) m
  getKatipNamespace = view nsLogNamespace
  localKatipNamespace f (NoBS m) = NoBS $ local (over nsLogNamespace f) m

runNoBS :: NoBSState -> NoBS a -> Handler a
runNoBS s m = 
  do
    gen <- liftIO getStdGen
    let ctx = s ^. nsLogContext
    let ns = s ^. nsLogNamespace
    let le = s ^. nsLogEnv
    runKatipContextT le ctx ns $ evalRandT (runReaderT (unNoBS m) s) gen

emptyState :: ServerState
emptyState = ServerState mempty mempty

addUser 
  :: ( MonadRandom m
     , MonadReader NoBSState m
     , MonadIO m
     , KatipContext m
     )
  => Connection 
  -> m Unique
addUser conn = 
  do
    st <- readIORef =<< view nsServerState
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
        mdf <- modifyIORef <$> view nsServerState
        mdf $ ssUserPool %~ insert uid newUser
        $(logTM) DebugS . ls $ "Added player " <> pname
        return uid

nobsAPI :: Proxy NoBSAPI
nobsAPI = Proxy

serveIndex :: ServerT Raw m
serveIndex = serveDirectoryFileServer "client"

serveSocket 
  :: ( MonadIO m
     , MonadReader NoBSState m
     , MonadRandom m
     , KatipContext m
     )
  => Connection 
  -> m ()
serveSocket conn =
  do
    $(logTM) InfoS "New connection!"
    liftIO $ sendTextData conn ("Welcome!" :: Text)
    uid <- addUser conn
    serveClient uid

serveClient 
  :: ( MonadIO m
     , MonadRandom m
     , MonadReader NoBSState m
     , KatipContext m
     )
  => Unique 
  -> m ()
serveClient uid =
  do
    st <- readIORef =<< view nsServerState
    let mconn = st ^? ssUserPool . at uid . _Just . userConn
    case mconn of
      Just x -> do
        conn <- takeMVar x
        msg <- liftIO $ receiveData conn
        putMVar x conn
        handleMsg uid msg
        serveClient uid
      Nothing -> $(logTM) ErrorS $ "Failed to get connection by uid " <> show uid

handleMsg 
  :: ( MonadIO m
     , MonadReader NoBSState m
     , MonadRandom m
     , KatipContext m
     )
  => Unique
  -> ByteString
  -> m ()
handleMsg userId msg = 
  do
    $(logTM) InfoS $ "Parsing message from " <> show userId
    case decode msg of
      Nothing  -> $(logTM) ErrorS $ "Failed to parse message: " <> show msg
      Just cmd -> doCommand userId cmd

doCommand 
  :: ( MonadIO m
     , MonadReader NoBSState m
     , MonadRandom m
     , KatipContext m
     )
  => Unique 
  -> ClientMsg 
  -> m ()
doCommand userId (CJoin r) = 
  do
    $(logTM) InfoS "Received a room join request"
    st <- getServerState
    let roomId = Unique r
    case st ^. ssRoomPool . at roomId of
      Just room -> 
        do
          let var = room ^. roomUsers
          usrs <- liftIO $ readMVar var
          res <- if userId `elem` usrs
            then do
              $(logTM) ErrorS "Player is already in room"
              return usrs
            else do
              $(logTM) InfoS "Sending connection data.."
              rd <- roomData room
              addUserToRoom userId roomId
              sendMessage userId $ SRoomData rd
              return $ cons userId usrs
          liftIO $ putMVar var res
      Nothing   -> $(logTM) ErrorS $ "Room " <> show roomId <> " does not exist."
doCommand userId CCreateRoom =
  do
    st <- getServerState
    roomId <- getRandom
    case st ^. ssRoomPool . at roomId of
      Just _  -> doCommand userId CCreateRoom
      Nothing -> 
        do
          userList <- newMVar []
          modifyServerState $ ssRoomPool %~ insert roomId (Room userList)
          $(logTM) InfoS $ "Created room " <> show roomId
          sendMessage userId (SRoomCreated $ show roomId)
doCommand _ _ = undefined

addUserToRoom
  :: ( MonadIO m
     , MonadReader NoBSState m
     , MonadRandom m
     )
  => Unique 
  -> Unique 
  -> m ()
addUserToRoom userId roomId =
  do
    undefined

roomData 
  :: ( MonadIO m
     , MonadReader NoBSState m
     )
  => Room 
  -> m RoomData
roomData room = 
  do
    userIds <- readMVar $ room ^. roomUsers
    players <- traverse getPlayer userIds
    return $ RoomData players

getUser 
  :: ( MonadReader NoBSState m
     , MonadIO m
     )
  => Unique 
  -> m User
getUser userId =
  do
    st <- getServerState
    case st ^. ssUserPool . at userId of
      Nothing -> error $ "User not found: " <> show userId
      Just u  -> return u

getConnection 
  :: ( MonadReader NoBSState m
     , MonadIO m
     )
  => Unique 
  -> m Connection
getConnection userId =
  do
    usr <- getUser userId
    readMVar $ usr ^. userConn

getPlayer
  :: ( MonadReader NoBSState m
     , MonadIO m
     )
  => Unique 
  -> m Player
getPlayer userId = return . Player $ show userId

sendMessage 
  :: ( MonadReader NoBSState m
     , MonadIO m
     )
  => Unique 
  -> ServerMsg 
  -> m ()
sendMessage userId msg = 
  do
    conn <- getConnection userId
    liftIO $ sendTextData conn $ encode msg

getServerState
  :: ( MonadIO m
     , MonadReader NoBSState m
     )
  => m ServerState
getServerState = readIORef =<< view nsServerState

modifyServerState
  :: ( MonadIO m
     , MonadReader NoBSState m
     )
  => (ServerState -> ServerState) -> m ()
modifyServerState f = (`modifyIORef` f) =<< view nsServerState

nobsServer 
  :: ( MonadIO m
     , MonadReader NoBSState m
     , MonadRandom m
     , KatipContext m
     )
  => ServerT NoBSAPI m
nobsServer = serveSocket
        :<|> const serveIndex
        :<|> serveIndex

app :: LogEnv -> NoBSState -> Application
app le s = serve nobsAPI $ hoistServer nobsAPI (runNoBS s) nobsServer

main :: IO ()
main = 
  do
    -- Set up logging
    handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    logEnv <- initLogEnv "NoBSServer" "production"
    let mkLogEnv :: IO LogEnv
        mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings logEnv
    bracket mkLogEnv closeScribes $ \le -> do
      runKatipContextT le () "main" $ do
        -- Generate Elm API module
        -- TODO move this to a different executable
        let modulePath = "client/src/NoBSAPI.elm" 
        $(logTM) InfoS $ "Generating Elm API module at " <> fromString modulePath
        writeFileText modulePath generateModule

        -- Start the server
        $(logTM) InfoS "Starting server at port 8080"
        ref <- newIORef emptyState
        let state = NoBSState
              { _nsServerState = ref
              , _nsLogEnv = le
              , _nsLogContext = mempty
              , _nsLogNamespace = "main"
              }
        liftIO . run 8080 $ app le state

