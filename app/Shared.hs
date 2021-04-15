{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Shared 
  ( ClientMsg(..), ServerMsg(..)
  , RoomData(..), Player(..)
  , Unique(..)
  , Rank(..), Suit(..), Card(..)
  , Bid(..)
  , pPlaying, pHand, pCards
  , writeTypes
  ) where

import Relude

import Control.Monad.Random
import Control.Lens

import Data.Aeson

import Language.PureScript.Bridge

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
  deriving (Enum, Eq, Ord, Show, Generic)

instance ToJSON Rank where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Rank

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Enum, Eq, Ord, Generic)

instance Show Suit where
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"
  show Spades   = "♠"

instance ToJSON Suit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Suit

data Card = Card 
  { _cRank :: Rank 
  , _cSuit :: Suit
  } deriving (Eq, Ord, Generic)

instance Show Card where
  show c = drop 1 $ show (_cRank c) <> show (_cSuit c)

instance Enum Card where
  fromEnum (Card r s) = fromEnum r * 4 + fromEnum s
  toEnum x = Card (toEnum $ x `div` 4) (toEnum $ x `mod` 4)

instance ToJSON Card where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Card

data Bid 
  = HighCard Rank
  | OnePair Rank
  | TwoPair Rank Rank
  | ThreeOfAKind Rank
  deriving (Generic)

instance ToJSON Bid where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Bid

newtype Unique = Unique Text
  deriving (Eq, Ord, Generic)

instance Show Unique where
  show (Unique x) = toString x

instance Random Unique where
  random g = (Unique $ show (x :: Word16), g')
    where (x, g') = random g

instance ToJSON Unique where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Unique

data Player = Player
  { _pUserID   :: Unique
  , _pUserName :: Text
  , _pSeat     :: Maybe Int
  , _pCards    :: Int
  , _pHand     :: [Card]
  , _pPlaying  :: Bool
  }
  deriving (Show, Generic)
makeLenses ''Player

instance ToJSON Player where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Player

data RoomData = RoomData
  { _rdPlayers :: [Player]
  }
  deriving (Show, Generic)

instance ToJSON RoomData where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RoomData

data ClientMsg
  = CJoin Text
  | CCreateRoom
  | CLeave
  | CSit Int
  | CSay Text
  | CRaise Int
  | CCall
  | CFold
  deriving (Show, Generic)

instance ToJSON ClientMsg where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ClientMsg

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
  deriving (Show, Generic)

instance ToJSON ServerMsg where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerMsg

writeTypes :: IO ()
writeTypes = 
  do
    writePSTypes "client/src/Shared.purs" (buildBridge defaultBridge) types
  where
    types =
      [ mkSumType (Proxy :: Proxy Rank)
      , mkSumType (Proxy :: Proxy Suit)
      , mkSumType (Proxy :: Proxy Card)
      , mkSumType (Proxy :: Proxy Bid)
      , mkSumType (Proxy :: Proxy Unique)
      , mkSumType (Proxy :: Proxy Player)
      , mkSumType (Proxy :: Proxy RoomData)
      , mkSumType (Proxy :: Proxy ClientMsg)
      , mkSumType (Proxy :: Proxy ServerMsg)
      ]

