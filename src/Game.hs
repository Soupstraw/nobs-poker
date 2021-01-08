{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Game 
  ( GameState
  , Player
  , Game.fold, call, raise
  , sit
  ) where

import Relude

import Control.Lens
import Control.Monad.Random
import Control.Monad.Except

import Data.Default
import Data.Vector as V

import System.Random.Shuffle

data GameException
  = InvalidSeatIndex
  | SeatOccupied
  | NoPlayers
  | NotEnoughMoney
  deriving (Show, Exception)

data Rank
  = R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | RJ
  | RQ
  | RK
  | RA
  deriving (Enum)

data Suit
  = Spades
  | Hearts
  | Clubs
  | Diamonds
  deriving (Enum)

data Card = Card Rank Suit

data Player = Player
  { _pMoney :: Int
  , _pBet   :: Int
  , _pState :: PlayerState
  }
  deriving (Eq)

data PlayerState
  = Playing
  | Checked
  | Folded
  | Leaving
  deriving (Eq)

data Table = Table
  { _tFlop  :: Maybe (Card, Card, Card)
  , _tTurn  :: Maybe Card
  , _tRiver :: Maybe Card
  }

instance Default Table where
  def = Table Nothing Nothing Nothing

data GameState = GameState
  { _gsSeats      :: Vector (Maybe Player)
  , _gsTurn       :: Int
  , _gsTable      :: Table
  , _gsDeck       :: [Card]
  , _gsDealer     :: Int
  , _gsSmallBlind :: Int
  , _gsBigBlind   :: Int
  }

deck :: [Card]
deck = Card <$> [R2 ..] <*> [Spades ..]

instance Default GameState where
  def = GameState
          { _gsSeats      = V.replicate 10 Nothing
          , _gsTurn       = 0
          , _gsTable      = def
          , _gsDeck       = deck
          , _gsDealer     = 0
          , _gsSmallBlind = 1
          , _gsBigBlind   = 2
          }

makePrisms ''PlayerState
makeLenses ''Player
makeLenses ''Table
makeLenses ''GameState

setupRound 
  :: ( MonadState GameState m
     , MonadError GameException m
     , MonadRandom m
     )
  => m ()
setupRound =
  do
    -- Remove players that have left and ready everyone else
    setupPlayers
    -- Move the dealer chip
    passDealer
    dealer <- use gsDealer
    gsTurn .= dealer
    passTurn
    -- Place blinds
    _smallBlind <- use gsSmallBlind
    _bigBlind <- use gsBigBlind
    raise _smallBlind
    raise _bigBlind
    -- Shuffle deck and deal cards
    _deck <- shuffleM deck
    gsDeck .= _deck

raise
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => Int
  -> m ()
raise x =
  do
    player <- whenNothingM (use currentPlayer) $ throwError InvalidSeatIndex
    when (player ^. pMoney < x) $ throwError NotEnoughMoney
    currentPlayer . _Just . pMoney -= x
    currentPlayer . _Just . pBet   += x
    passTurn

call
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
call =
  do
    player <- whenNothingM (use currentPlayer) $ throwError InvalidSeatIndex
    mbyMaxBet <- maximumOf (gsSeats . folded . _Just . pBet) <$> get
    let maxBet = fromMaybe (error "No seats found") mbyMaxBet
    let playerBet = player ^. pBet
    let playerMoney = player ^. pMoney
    let betAmt = min (maxBet - playerBet) playerMoney
    currentPlayer . _Just . pMoney -= betAmt
    currentPlayer . _Just . pBet   += betAmt
    passTurn

fold
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
fold =
  do
    currentPlayer . _Just . pState .= Folded
    passTurn

passIndex 
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => Lens' GameState Int -> m ()
passIndex l =
  do
    whenM playersPlaying $ throwError NoPlayers
    seats <- use gsSeats
    let idxUp 0 = throwError NoPlayers
        idxUp i =
          do
            l %= (\x -> (x + 1) `mod` V.length seats)
            idx <- use l
            unlessM (has (gsSeats . ix idx . _Just) <$> get) . idxUp $ i - 1
    idxUp $ V.length seats

passTurn
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
passTurn = passIndex gsTurn

passDealer
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
passDealer = passIndex gsDealer

playerByIndex :: (GameState -> Int) -> Lens' GameState (Maybe Player)
playerByIndex idx = lens getter setter
  where
    getter :: GameState -> Maybe Player
    getter gs = gs ^? gsSeats . ix (idx gs) . _Just
    setter :: GameState -> Maybe Player -> GameState
    setter gs y = gs & gsSeats . ix (idx gs) .~ y

currentPlayer :: Lens' GameState (Maybe Player)
currentPlayer = playerByIndex $ view gsTurn

currentDealer :: Lens' GameState (Maybe Player)
currentDealer = playerByIndex $ view gsDealer

playersPlaying :: MonadState GameState m => m Bool
playersPlaying = 
  do
    seats <- use gsSeats
    return $ has (folded . _Just . pState . _Playing) seats

setupPlayers 
  :: ( MonadState GameState m
     )
  => m ()
setupPlayers =
  do
    -- Remove players that have left
    gsSeats . traversed . filtered (has $ _Just . pState . _Leaving) .= Nothing
    -- Ready everyone else
    gsSeats . traversed . _Just . pState .= Playing

sit
  :: ( MonadState GameState m
     , MonadError GameException m
     )
  => Player -> Int -> m ()
sit p i = 
  do
    seats <- use gsSeats
    when (has (ix i . _Just) seats) $ throwError SeatOccupied
    gsSeats . ix i .= Just p

