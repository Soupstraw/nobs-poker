{-# LANGUAGE TemplateHaskell #-}

module Shared 
  ( ClientMsg(..), ServerMsg(..)
  , RoomData(..), Player(..)
  , Unique(..)
  , Rank(..), Suit(..), Card(..)
  , Bid(..)
  , generateModule
  , pPlaying, pHand, pCards
  ) where

import Relude

import Elm.Derive
import Elm.Module

import Control.Monad.Random
import Control.Lens

import qualified Text.Show as T

data Rank
  = R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | RT
  | RJ
  | RQ
  | RK
  | RA
  deriving (Enum, Eq, Ord, Show)
deriveBoth (defaultOptionsDropLower 1) ''Rank

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Enum, Eq, Ord)
deriveBoth (defaultOptionsDropLower 1) ''Suit

instance Show Suit where
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"
  show Spades   = "♠"

data Card = Card 
  { _cRank :: Rank 
  , _cSuit :: Suit
  } deriving (Eq, Ord)
deriveBoth (defaultOptionsDropLower 1) ''Card

instance Show Card where
  show c = drop 1 $ show (_cRank c) <> show (_cSuit c)

instance Enum Card where
  fromEnum (Card r s) = fromEnum r * 4 + fromEnum s
  toEnum x = Card (toEnum $ x `div` 4) (toEnum $ x `mod` 4)

data Bid 
  = HighCard Rank
  | OnePair Rank
  | TwoPair Rank Rank
  | ThreeOfAKind Rank

newtype Unique = Unique Text
  deriving (Eq, Ord)
deriveBoth (defaultOptionsDropLower 1) ''Unique

instance Show Unique where
  show (Unique x) = toString x

instance Random Unique where
  random g = (Unique $ show (x :: Word16), g')
    where (x, g') = random g

data Player = Player
  { _pUserID   :: Unique
  , _pUserName :: Text
  , _pSeat     :: Maybe Int
  , _pCards    :: Int
  , _pHand     :: [Card]
  , _pPlaying  :: Bool
  }
  deriving (Show)
deriveBoth (defaultOptionsDropLower 1) ''Player
makeLenses ''Player

data RoomData = RoomData
  { _rdPlayers :: [Player]
  }
  deriving (Show)
deriveBoth (defaultOptionsDropLower 1) ''RoomData

data ClientMsg
  = CJoin Text
  | CCreateRoom
  | CLeave
  | CSit Int
  | CSay Text
  | CRaise Int
  | CCall
  | CFold
  deriving (Show)
deriveBoth (defaultOptionsDropLower 1) ''ClientMsg

data ServerMsg
  = SRoomData RoomData
  | SJoin Player
  | SSay Unique Text
  | SLeave Unique
  | SSit Unique Int
  | SRaise Unique Int
  | SCall Unique
  | SFold Unique
  | SDrawCards
  | SRoomCreated Unique
  deriving (Show)
deriveBoth (defaultOptionsDropLower 1) ''ServerMsg

generateModule :: Text
generateModule = toText $ makeElmModule "NoBSAPI"
  [ DefineElm (Proxy :: Proxy ClientMsg)
  , DefineElm (Proxy :: Proxy ServerMsg)
  , DefineElm (Proxy :: Proxy Player)
  , DefineElm (Proxy :: Proxy RoomData)
  , DefineElm (Proxy :: Proxy Unique)
  , DefineElm (Proxy :: Proxy Rank)
  , DefineElm (Proxy :: Proxy Suit)
  , DefineElm (Proxy :: Proxy Card)
  ]

