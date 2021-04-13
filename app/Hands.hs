module Hands 
  ( deck52, deck24
  , Bid(..)
  ) where

import Relude

import Data.List as L
import Data.Maybe

import Shared

--import Debug.Trace

follows :: Card -> Card -> Bool
follows x y
  | _cRank x == R2 && _cRank y == RA = True
  | _cRank y == RA = False
  | otherwise = _cRank x == succ (_cRank y)

deck24 :: [Card]
deck24 = drop 28 deck52

deck52 :: [Card]
deck52 = Card <$> [R2 ..] <*> [Clubs ..]

