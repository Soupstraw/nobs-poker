{-# LANGUAGE TemplateHaskell #-}

module Shared 
  ( ClientMsg(..), ServerMsg(..)
  , RoomData(..), Player(..)
  , generateModule
  ) where

import Relude

import Elm.Derive
import Elm.Module

data Player = Player
  { pUserID   :: Text
  , pUserName :: Text
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
  | SSay Player Text
  | SLeave Text
  | SSit Text Int
  | SRaise Text Int
  | SCall Text
  | SFold Text
  | SDrawCards
  | SRoomCreated Text
  deriving (Show)
deriveBoth defaultOptions ''ServerMsg

generateModule :: Text
generateModule = toText $ makeElmModule "NoBSAPI"
  [ DefineElm (Proxy :: Proxy ClientMsg)
  , DefineElm (Proxy :: Proxy ServerMsg)
  , DefineElm (Proxy :: Proxy Player)
  , DefineElm (Proxy :: Proxy RoomData)
  ]

