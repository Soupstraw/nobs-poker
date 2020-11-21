{-# LANGUAGE TemplateHaskell #-}

module Shared 
  ( ClientMsg(..), ServerMsg(..)
  , generateModule
  ) where

import Relude

import Elm.Derive
import Elm.Module

data Player = Player
  { pName   :: Text
  , pUserID :: Text
  }
  deriving (Show)
deriveBoth defaultOptions ''Player

data ClientMsg
  = CJoin Text
  | CLeave
  | CSit Int
  | CSay Text
  | CRaise Int
  | CCall
  | CFold
  deriving (Show)
deriveBoth defaultOptions ''ClientMsg

data ServerMsg
  = SList [Text]
  | SJoin Player
  | SLeave Text
  | SSit Text Int
  | SRaise Text Int
  | SCall Text
  | SFold Text
  | SDrawCards
deriveBoth defaultOptions ''ServerMsg

generateModule :: Text
generateModule = toText $ makeElmModule "NoBSAPI"
  [ DefineElm (Proxy :: Proxy ClientMsg)
  , DefineElm (Proxy :: Proxy ServerMsg)
  , DefineElm (Proxy :: Proxy Player)
  ]

