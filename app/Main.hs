{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Relude hiding (ByteString)
import Relude.Extra.Map

import Control.Concurrent.MVar (isEmptyMVar, modifyMVar_)
import Control.Exception (bracket)
import Control.Monad.Catch (MonadThrow, MonadCatch, catch, throwM)
import Control.Lens hiding (Fold, (??))
import Control.Monad.Random

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Map ()

import Katip

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

import Shared

import Game

data User = User
  { _userName  :: MVar Text
  , _userConn  :: MVar Connection
  , _userMoney :: Int
  , _userRoom  :: MVar (Maybe Unique)
  , _userSeat  :: MVar (Maybe Int)
  }
makeLenses 'User

data Room = Room
  { _roomUsers :: MVar [Unique]
  , _roomGame  :: GameState
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

instance MonadThrow NoBS where
  throwM = liftIO . throwM

instance MonadCatch NoBS where
  catch action handler =
    do
      st <- ask
      res <- liftIO . catch (runHandler $ runNoBS st action) $ \ex ->
        do
          runHandler . runNoBS st $ handler ex
      case res of
        Left ex -> throwM ex
        Right x -> return x

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
        mvarRoom <- newMVar Nothing
        mvarSeatIdx <- newMVar Nothing
        let newUser = User mvarName mvarConn 0 mvarRoom mvarSeatIdx
        mdf <- modifyIORef <$> view nsServerState
        mdf $ ssUserPool %~ insert uid newUser
        $(logTM) DebugS . ls $ "Added player " <> pname
        return uid

removeUser 
  :: ( MonadReader NoBSState m
     , MonadIO m
     , MonadRandom m
     , KatipContext m
     ) 
  => Unique 
  -> m ()
removeUser uid =
  do
    usr <- getUser uid
    
    -- Tell everyone in the room that the player left
    mbyRoomId <- readMVar $ usr ^. userRoom
    whenJust mbyRoomId $ sendRoomMessage ?? SLeave uid

    -- Remove the player from the user pool
    mdf <- modifyIORef <$> view nsServerState
    mdf $ ssUserPool %~ delete uid
    $(logTM) InfoS $ "Player " <> show uid <> " left"

nobsAPI :: Proxy NoBSAPI
nobsAPI = Proxy

serveIndex :: ServerT Raw m
serveIndex = serveDirectoryFileServer "client"

serveSocket 
  :: ( MonadIO m
     , MonadReader NoBSState m
     , MonadRandom m
     , KatipContext m
     , MonadCatch m
     )
  => Connection 
  -> m ()
serveSocket conn =
  do
    $(logTM) InfoS "New connection!"
    uid <- addUser conn
    catch (serveClient uid) $ \ex -> do
      case ex of
        CloseRequest{} -> $(logTM) InfoS "Connection closed!"
        x -> $(logTM) WarningS $ "Exception: " <> show x
      removeUser uid

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
      Just var -> do
        notifyEmpty var
        conn <- readMVar var
        msg <- liftIO $ receiveData conn
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
    $(logTM) InfoS $ "Received message from " <> show userId
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
          notifyEmpty var
          usrs <- liftIO $ readMVar var
          res <- if userId `elem` usrs
            then do
              $(logTM) ErrorS "Player is already in room"
              return usrs
            else do
              conn <- getConnection userId
              rd <- roomData room
              sendMessage conn $ SRoomData rd
              addUserToRoom userId roomId
              $(logTM) DebugS "User added to room"
              return $ cons userId usrs
          void . liftIO $ swapMVar var res
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
          modifyServerState $ ssRoomPool %~ insert roomId (Room userList defaultState)
          $(logTM) InfoS $ "Created room " <> show roomId
          conn <- getConnection userId
          sendMessage conn (SRoomCreated roomId)
doCommand userId (CSay msg) = 
  do
    usr <- getUser userId
    let var = usr ^. userRoom
    notifyEmpty var
    mbyRoomId <- readMVar var
    case mbyRoomId of
      Just roomId ->
        do
          sendRoomMessage roomId $ SSay userId msg
      Nothing ->
        $(logTM) ErrorS "User is not in a room"
doCommand userId (CSit seatIdx) =
  do
    user <- getUser userId
    curSeat <- readMVar $ user ^. userSeat

    -- Check whether the user is already sitting at the table
    if isJust curSeat
      then $(logTM) ErrorS "User is already sitting at the table"
      else do
        mbyRoomId <- readMVar $ user ^. userRoom
        -- Check whether the user is in a room
        case mbyRoomId of
          Just roomId ->
            do
              room <- getRoom roomId
              others <- readMVar $ room ^. roomUsers
              -- Check whether the seat is taken
              taken <- anyM ?? others $ \u -> do
                o <- getUser u
                seat <- readMVar $ o^. userSeat
                return $ seat == Just seatIdx
              if taken
                then $(logTM) ErrorS "Seat is already taken"
                else 
                  do
                    liftIO . modifyMVar_ (user ^. userSeat) . const . return $ Just seatIdx
                    sendRoomMessage roomId $ SSit userId seatIdx
          Nothing -> $(logTM) ErrorS "User is not in a room"
doCommand _ c = $(logTM) ErrorS $ "Cannot handle command " <> show c <> " yet."

addUserToRoom
  :: ( MonadIO m
     , MonadReader NoBSState m
     , MonadRandom m
     , KatipContext m
     )
  => Unique 
  -> Unique 
  -> m ()
addUserToRoom userId roomId =
  do
    $(logTM) DebugS "Adding user to room"
    room <- getRoom roomId
    let var = room ^. roomUsers

    usr <- getUser userId
    liftIO . modifyMVar_ (usr ^. userRoom) . const . return $ Just roomId

    -- Update room players list serverside
    notifyEmpty var
    void . liftIO . modifyMVar_ var $ return . cons userId

    -- Tell everyone in the room that the player joined
    player <- getPlayer userId
    sendRoomMessage roomId $ SJoin player

notifyEmpty :: (MonadIO m, KatipContext m) => MVar a -> m ()
notifyEmpty var =
  do
    cond <- liftIO $ isEmptyMVar var
    when cond $ $(logTM) DebugS "Blocking on MVar"

sendRoomMessage 
  :: ( MonadIO m
     , MonadReader NoBSState m
     , MonadRandom m
     , KatipContext m
     )
  => Unique 
  -> ServerMsg 
  -> m ()
sendRoomMessage roomId msg =
  do
    room <- getRoom roomId
    let var = room ^. roomUsers
    notifyEmpty var
    usrs <- readMVar var
    forM_ usrs $ \u -> do
      conn <- getConnection u
      sendMessage conn msg

roomData 
  :: ( MonadIO m
     , MonadReader NoBSState m
     , KatipContext m
     )
  => Room 
  -> m RoomData
roomData room = 
  do
    let var = room ^. roomUsers
    notifyEmpty var
    userIds <- readMVar var
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

getRoom 
  :: ( MonadReader NoBSState m
     , MonadIO m
     )
  => Unique
  -> m Room
getRoom roomId =
  do 
    st <- getServerState
    case st ^. ssRoomPool . at roomId of
      Nothing -> error $ "Room not found: " <> show roomId
      Just r  -> return r

getConnection 
  :: ( MonadReader NoBSState m
     , MonadIO m
     , KatipContext m
     )
  => Unique 
  -> m Connection
getConnection userId =
  do
    usr <- getUser userId
    let var = usr ^. userConn
    notifyEmpty var
    readMVar var

getPlayer
  :: ( MonadReader NoBSState m
     , MonadIO m
     )
  => Unique 
  -> m Shared.Player
getPlayer userId = 
  do
    player <- getUser userId
    pName <- readMVar $ player ^. userName
    let money = player ^. userMoney
    seat <- readMVar $ player ^. userSeat
    return $ Player userId pName money seat

sendMessage 
  :: ( MonadReader NoBSState m
     , MonadIO m
     , KatipContext m
     )
  => Connection 
  -> ServerMsg 
  -> m ()
sendMessage conn msg = 
  do
    $(logTM) DebugS $ "Sending message: " <> show msg
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
     , MonadCatch m
     )
  => ServerT NoBSAPI m
nobsServer = serveSocket
        :<|> const serveIndex
        :<|> serveIndex

app :: NoBSState -> Application
app s = serve nobsAPI $ hoistServer nobsAPI (runNoBS s) nobsServer

main :: IO ()
main = 
  do
    -- Set up logging
    handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
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
        let st = NoBSState
              { _nsServerState = ref
              , _nsLogEnv = le
              , _nsLogContext = mempty
              , _nsLogNamespace = "main"
              }
        liftIO . run 8080 $ app st

