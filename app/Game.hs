{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Game 
  ( GameState
  , Player
  , sit
  , callBluff
  , raise
  , startRound
  ) where

import Relude

import Control.Lens
import Control.Monad.Random (MonadRandom)
import Control.Monad.Except (MonadError, throwError)

import Data.Vector as V (Vector, replicate, length, (!), (//))
import Data.Default

import System.Random.Shuffle

import Hands
import Shared

data GameException
  = InvalidSeatIndex
  | SeatOccupied
  | NoPlayers
  | EmptyDeck
  | BadRaise
  | BadBluffCall
  deriving (Show, Exception)

data GameStage
  = Lobby
  | Playing

data GameState = GameState
  { _gsSeats      :: Vector (Maybe Player)
  , _gsTurn       :: Int
  , _gsHighestBid :: Maybe Bid
  , _gsOfferIdx   :: Int
  , _gsDeck       :: [Card]
  , _gsStage      :: GameStage
  }

instance Default GameState where
  def = GameState
          { _gsSeats      = V.replicate 8 Nothing
          , _gsTurn       = 0
          , _gsDeck       = deck24
          , _gsStage      = Lobby
          , _gsHighestBid = Nothing
          , _gsOfferIdx   = 0
          }
makeLenses ''GameState

passTurn 
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => m ()
passTurn =
  do
    whenM ((>0) <$> playersPlaying) $ throwError NoPlayers
    seats <- use gsSeats
    offerIdx <- use gsTurn
    gsOfferIdx .= offerIdx
    gsTurn += 1
    gsTurn %= (`mod` V.length seats)
    unlessM (has currentPlayer <$> get) passTurn

sit 
  :: ( MonadState GameState m
     , MonadError GameException m
     ) 
  => Player 
  -> Int 
  -> m ()
sit p i = 
  do
    _seats <- use gsSeats
    unless (i >= 0) $ throwError InvalidSeatIndex
    unless (i < V.length _seats) $ throwError InvalidSeatIndex
    unless (isNothing $ _seats ! i) $ throwError SeatOccupied
    gsSeats %= (// [(i, Just p)])
    curPlayer <- use currentPlayer
    when (isNothing curPlayer) $ gsTurn .= i

startRound 
  :: ( MonadState GameState m
     , MonadRandom m
     )
  => m ()
startRound =
  do
    deck <- use gsDeck
    shuffled <- shuffleM deck
    gsDeck .= shuffled
    seats <- use gsSeats
    seats' <- forM seats $ \p' -> do
      case p' of
        Just p  -> 
          do
            let nCards = p ^. pCards
            cards <- draw nCards
            return . Just $ p & pHand .~ cards
        Nothing -> return Nothing
    gsSeats .= seats'

callBluff 
  :: ( MonadState GameState m
     , MonadError GameException m
     , MonadRandom m
     )
  => m ()
callBluff = 
  do
    undefined

raise 
  :: ( MonadState GameState m
     , MonadError GameException m
     )
  => Bid -> m ()
raise hand = 
  do
    undefined

playersPlaying :: MonadState GameState m => m Int
playersPlaying = 
  do
    seats <- use gsSeats
    return . sum $ seats ^.. folded . _Just . pPlaying . to fromEnum

playerByIndex :: (GameState -> Int) -> Lens' GameState (Maybe Player)
playerByIndex idx = lens getter setter
  where
    getter :: GameState -> Maybe Player
    getter gs = gs ^? gsSeats . ix (idx gs) . _Just
    setter :: GameState -> Maybe Player -> GameState
    setter gs y = gs & gsSeats . ix (idx gs) .~ y

currentPlayer :: Lens' GameState (Maybe Player)
currentPlayer = playerByIndex $ view gsTurn

offerPlayer :: Lens' GameState (Maybe Player)
offerPlayer = playerByIndex $ view gsOfferIdx

draw 
  :: ( MonadState GameState m
     )
  => Int -> m [Card]
draw n =
  do
    deck <- use gsDeck
    gsDeck %= drop n
    return $ take n deck

