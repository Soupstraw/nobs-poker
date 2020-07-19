{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Game 
  (
  ) where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Random

import Control.Exception

import Data.Vector as V
import Data.Maybe

import System.Random.Shuffle

data GameException
  = InvalidSeatIndex
  | SeatOccupied
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

deck :: [Card]
deck = Card <$> [R2 ..] <*> [Spades ..]

sit 
  :: (Monad m, MonadState GameState m) 
  => Player 
  -> Int 
  -> m ()
sit p i = 
  do
    _seats <- use gsSeats
    when (i >= 0) $ throw InvalidSeatIndex
    when (i < V.length _seats) $ throw InvalidSeatIndex
    when (_seats ! i == Nothing) $ throw SeatOccupied
    gsSeats %= (// [(i, Just p)])

setupRound 
  :: (Monad m, MonadState GameState m, MonadRandom m)
  => m ()
setupRound =
  do
    -- Move the dealer chip
    _seats <- use gsSeats
    gsDealer %= nextPlayerIdx _seats
    _dealer <- use gsDealer
    gsTurn .= nextPlayerIdx _seats _dealer
    -- Place blinds
    _smallBlind <- use gsSmallBlind
    _bigBlind <- use gsBigBlind
    raise _smallBlind
    raise _bigBlind
    -- Shuffle deck and deal cards
    _deck <- shuffleM deck
    gsDeck .= _deck

nextPlayerIdx :: Vector (Maybe Player) -> Int -> Int
nextPlayerIdx vec i
  | not $ playersPlaying vec = error "No players at the table"
  | fromMaybe False $ nextP ^? _Just . pPlaying = next
  | otherwise = nextPlayerIdx vec next
  where next = i+1 `mod` V.length vec
        nextP = vec V.! next

raise
  :: (Monad m, MonadState GameState m)
  => Int
  -> m ()
raise x =
  do
    passTurn

call
  :: (Monad m, MonadState GameState m)
  => m ()
call =
  do
    passTurn

fold
  :: (Monad m, MonadState GameState m)
  => m ()
fold =
  do
    passTurn

passTurn 
  :: (Monad m, MonadState GameState m)
  => m ()
passTurn =
  do
    _seats <- use gsSeats
    gsTurn %= nextPlayerIdx _seats

currentPlayer :: m Player
currentPlayer = undefined

playersPlaying :: Vector (Maybe Player) -> Bool
playersPlaying = undefined

