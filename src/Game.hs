{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Game 
  ( GameState
  , Player
  , defaultState
  , raise, Game.fold, call
  ) where

import Relude

import Control.Lens
import Control.Monad.Random
import Control.Monad.Except

import Control.Exception

import Data.Vector as V

import System.Random.Shuffle

data GameException
  = InvalidSeatIndex
  | SeatOccupied
  | NoPlayers
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
  { _pMoney   :: Int
  , _pPlaying :: Bool
  }
  deriving (Eq)

data GameState = GameState
  { _gsSeats      :: Vector (Maybe Player)
  , _gsTurn       :: Int
  , _gsTable      :: [Card]
  , _gsDeck       :: [Card]
  , _gsDealer     :: Int
  , _gsSmallBlind :: Int
  , _gsBigBlind   :: Int
  }

type Holdem = ExceptT GameException (StateT GameState IO)

makeLenses ''Player
makeLenses ''GameState

defaultState :: GameState
defaultState = GameState
  { _gsSeats      = V.replicate 10 Nothing
  , _gsTurn       = 0
  , _gsTable      = []
  , _gsDeck       = deck
  , _gsDealer     = 0
  , _gsSmallBlind = 1
  , _gsBigBlind   = 2
  }

deck :: [Card]
deck = Card <$> [R2 ..] <*> [Spades ..]

sit 
  :: (MonadState GameState m) 
  => Player 
  -> Int 
  -> m ()
sit p i = 
  do
    _seats <- use gsSeats
    when (i >= 0) $ throw InvalidSeatIndex
    when (i < V.length _seats) $ throw InvalidSeatIndex
    when (isNothing $ _seats ! i) $ throw SeatOccupied
    gsSeats %= (// [(i, Just p)])

setupRound 
  :: ( MonadState GameState m
     , MonadError GameException m
     , MonadRandom m
     )
  => m ()
setupRound =
  do
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
    passTurn

call
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
call =
  do
    passTurn

fold
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
fold =
  do
    passTurn

passTurn 
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
passTurn =
  do
    whenM playersPlaying $ throwError NoPlayers
    seats <- use gsSeats
    gsTurn += 1
    gsTurn %= (`mod` V.length seats)
    void currentPlayer `catchError` (\InvalidSeatIndex -> passTurn)

passDealer
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
passDealer =
  do
    whenM playersPlaying $ throwError NoPlayers
    seats <- use gsSeats
    gsDealer += 1
    gsDealer %= (`mod` V.length seats)
    void currentDealer `catchError` (\InvalidSeatIndex -> passDealer)

currentPlayer 
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m Player
currentPlayer = 
  do
    seats <- use gsSeats
    turn <- use gsTurn
    case seats V.! turn of
      Just x  -> return x
      Nothing -> throwError InvalidSeatIndex

currentDealer
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m Player
currentDealer =
  do
    seats <- use gsSeats
    dealer <- use gsDealer
    case seats V.! dealer of
      Just x  -> return x
      Nothing -> throwError InvalidSeatIndex

playersPlaying :: MonadState GameState m => m Bool
playersPlaying = 
  do
    seats <- use gsSeats
    return . Relude.or $ seats ^.. folded . _Just . pPlaying

