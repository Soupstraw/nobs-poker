{-# LANGUAGE TemplateHaskell #-}

module Shared 
  ( ClientMsg(..), ServerMsg(..)
  , RoomData(..), Player(..)
  , Unique(..)
  , generateModule
  ) where

import Relude

import Elm.Derive
import Elm.Module

import Control.Monad.Random

import Data.Aeson hiding (defaultOptions)

import qualified Text.Show as T

newtype Unique = Unique Text
  deriving (Eq, Ord)
deriveBoth defaultOptions ''Unique

instance Show Unique where
  show (Unique x) = toString x

instance Random Unique where
  random g = (Unique $ show (x :: Word16), g')
    where (x, g') = random g

data Player = Player
  { pUserID   :: Unique
  , pUserName :: Text
  , pMoney    :: Int
  , pSeat     :: Maybe Int
  }
  deriving (Show)
deriveBoth defaultOptions ''Player

data RoomData = RoomData
  { rdPlayers :: [Player]
  }
  deriving (Show)
deriveBoth defaultOptions ''RoomData

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
deriveBoth defaultOptions ''ClientMsg

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
deriveBoth defaultOptions ''ServerMsg

generateModule :: Text
generateModule = toText $ makeElmModule "NoBSAPI"
  [ DefineElm (Proxy :: Proxy ClientMsg)
  , DefineElm (Proxy :: Proxy ServerMsg)
  , DefineElm (Proxy :: Proxy Player)
  , DefineElm (Proxy :: Proxy RoomData)
  , DefineElm (Proxy :: Proxy Unique)
  ]

