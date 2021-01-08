{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Relude hiding (ByteString)
import Relude.Extra.Map

import Control.Exception (bracket)
import Control.Monad.Catch (MonadThrow, MonadCatch, catch, throwM)
import Control.Lens hiding (Fold, (??))
import Control.Monad.Random

import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Map ()

import Katip

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets

import Servant
import Servant.API.WebSocket

import Shared as S

import Game

data User = User
  { _userName  :: Text
  , _userConn  :: Connection
  , _userMoney :: Int
  , _userRoom  :: Maybe Unique
  , _userSeat  :: Maybe Int
  }
makeLenses 'User

data Room = Room
  { _roomUsers :: [Unique]
  , _roomGame  :: GameState
  }
makeLenses 'Room

data ServerState = ServerState
  { _ssUserPool :: Map Unique User
  , _ssRoomPool :: Map Unique Room
  } 
makeLenses 'ServerState

instance Default ServerState where
  def = ServerState def def

type NoBSAPI = "socket" :> WebSocket
          :<|> "room" :> Capture "roomId" Text :> Raw
          :<|> Raw

data NoBSState = NoBSState
  { _nsServerState  :: TVar ServerState
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

addUser 
  :: ( MonadRandom m
     , MonadReader NoBSState m
     , MonadState ServerState m
     , MonadIO m
     , KatipContext m
     )
  => Connection 
  -> m Unique
addUser conn = 
  do
    userPool <- use ssUserPool
    uid <- getRandom
    if member uid userPool
      then addUser conn
      else do
        let pname = "Player#" <> show uid
        let newUser = User pname conn 0 Nothing Nothing
        $(logTM) DebugS . ls $ "Added player " <> pname
        modify $ ssUserPool %~ insert uid newUser
        return uid

removeUser 
  :: ( MonadState ServerState m
     , MonadIO m
     , MonadRandom m
     , KatipContext m
     ) 
  => Unique 
  -> m ()
removeUser uid = 
  do
    mbyUsr <- use $ ssUserPool . at uid
    
    case mbyUsr of
      Just usr ->
        do
          -- Tell everyone in the room that the player left
          let mbyRoomId = usr ^. userRoom
          whenJust mbyRoomId $ sendRoomMessage ?? SLeave uid

          -- Remove the player from the user pool
          ssUserPool %= delete uid
          $(logTM) InfoS $ "Player " <> show uid <> " left"
      Nothing -> $(logTM) ErrorS $ "User not found: " <> show uid

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
    mconn <- atomically $ do
      st <- readTVar =<< view nsServerState
      return $ st ^? ssUserPool . at uid . _Just . userConn
    case mconn of
      Just conn -> do
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
    let roomId = Unique r
    room' <- use $ ssRoomPool . at roomId
    case room' of
      Just room -> 
        do
          let usrs = room ^. roomUsers
          res <- if userId `elem` usrs
            then do
              $(logTM) ErrorS "Player is already in room"
              return usrs
            else do
              conn <- use $ ssUserPool . at userId . _Just . userConn
              rd <- roomData room
              sendMessage conn $ SRoomData rd
              addUserToRoom userId roomId
              $(logTM) DebugS "User added to room"
              return $ cons userId usrs
          ssRoomPool . at roomId . _Just . roomUsers .= res
      Nothing   -> $(logTM) ErrorS $ "Room " <> show roomId <> " does not exist."
doCommand userId CCreateRoom =
  do
    roomId <- getRandom
    room <- use $ ssRoomPool . at roomId
    case room of
      Just _  -> doCommand userId CCreateRoom
      Nothing -> 
        do
          ssRoomPool %= insert roomId (Room [] def)
          $(logTM) InfoS $ "Created room " <> show roomId
          conn <- use $ ssUserPool . at userId . _Just . userConn
          sendMessage conn (SRoomCreated roomId)
doCommand userId (CSay msg) = 
  do
    usr <- use $ ssUserPool . ix userId
    let mbyRoomId = usr ^. userRoom
    case mbyRoomId of
      Just roomId ->
        do
          sendRoomMessage roomId $ SSay userId msg
      Nothing ->
        $(logTM) ErrorS "User is not in a room"
doCommand userId (CSit seatIdx) = atomicallyWithState $
  do
    mbyUser <- use $ ssUserPool . at userId
    user <- whenNothing mbyUser undefined
    let roomId = user ^. userRoom . _Just
    let player = undefined
    zoom (ssRoomPool . ix roomId . roomGame) $ sit player seatIdx
doCommand _ c = $(logTM) ErrorS $ "Cannot handle command " <> show c <> " yet."

addUserToRoom
  :: ( MonadIO m
     , MonadState ServerState m
     , MonadRandom m
     , KatipContext m
     )
  => Unique 
  -> Unique 
  -> m ()
addUserToRoom userId roomId =
  do
    $(logTM) DebugS "Adding user to room"
    atomically $ do

      whenM (hasn't (ssUserPool . at userId . _Just . userRoom) <$> get) $ throwSTM ""
      ssUserPool . at userId . _Just . userRoom ?= roomId

      -- Update room players list serverside
      whenM (hasn't (ssRoomPool . at roomId . _Just . roomUsers) <$> get) $ throwSTM ""
      ssRoomPool . at roomId . _Just . roomUsers %= cons userId

    -- Tell everyone in the room that the player joined
    player <- getPlayer userId
    sendRoomMessage roomId $ SJoin player

sendRoomMessage 
  :: ( MonadIO m
     , MonadState ServerState m
     , MonadRandom m
     , KatipContext m
     )
  => Unique 
  -> ServerMsg 
  -> m ()
sendRoomMessage roomId msg =
  do
    room <- use $ ssRoomPool . ix roomId
    let usrs = room ^. roomUsers
    forM_ usrs $ \u -> do
      conn <- use $ ssUserPool . ix u . userConn
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
    let userIds = room ^. roomUsers
    players <- traverse getPlayer userIds
    return $ RoomData players

getPlayer
  :: ( MonadReader NoBSState m
     , MonadIO m
     )
  => Unique 
  -> m S.Player
getPlayer userId = 
  do
    player <- use $ ssUserPool . ix userId
    let pName = player ^. userName
    let money = player ^. userMoney
    let seat = player ^. userSeat
    return $ Player userId pName money seat

sendMessage 
  :: ( MonadState ServerState m
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
        ref <- newTVarIO def
        let st = NoBSState
              { _nsServerState = ref
              , _nsLogEnv = le
              , _nsLogContext = mempty
              , _nsLogNamespace = "main"
              }
        liftIO . run 8080 $ app st

atomicallyWithState 
  :: ( MonadIO m
     , MonadReader NoBSState m
     ) 
  => StateT ServerState STM a -> m a
atomicallyWithState act = atomically $ 
  do
    var <- view nsServerState
    st <- readTVar var
    (a, st') <- runStateT act st
    writeTVar var st'
    return a

