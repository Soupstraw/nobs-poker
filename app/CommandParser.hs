{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CommandParser 
  ( parseMessage
  , UserMessage(..)
  , umRoom, umAction
  , UserAction(..)

  ) where

import Data.Foldable
import Data.String (IsString)
import qualified Data.Text.Lazy as T

import Control.Lens

import Network.WebSockets

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec CommandParseError T.Text

data UserMessage = UserMessage
  { _umRoom   :: Int
  , _umAction :: UserAction
  }

data UserAction 
  = SetName T.Text
  | JoinRoom Int
  | Say T.Text
  | ParseError T.Text

newtype CommandParseError = CommandParseError 
  {unCommandParseError :: T.Text}
  deriving (Eq, Ord, IsString)

makeLenses ''UserMessage

instance ShowErrorComponent CommandParseError where
  showErrorComponent = T.unpack . unCommandParseError

instance WebSocketsData UserMessage where
  fromDataMessage (Text _ (Just msg)) = parseMessage msg
  fromLazyByteString = undefined
  toLazyByteString = undefined

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseMessage :: T.Text -> UserMessage
parseMessage msg = case parse messageParser "" msg of
  Left e  -> parseErrorMsg . T.pack $ errorBundlePretty e
  Right x -> x

messageParser :: Parser UserMessage
messageParser =
  do
    roomId <- lexeme L.decimal
    act <- actionParser
    return $ UserMessage roomId act

actionParser :: Parser UserAction
actionParser = 
  do
    try $ char '/'
    act <- asum 
      [ try $ setNameParser
      , try $ joinRoomParser
      , customFailure "Unknown command"
      ]
    return act
  <|> (Say <$> takeRest)

setNameParser :: Parser UserAction
setNameParser =
  do
    symbol "setname"
    n <- lexeme $ count' 3 16 asciiChar
    eof
    return . SetName . T.pack $  n

joinRoomParser :: Parser UserAction
joinRoomParser =
  do
    symbol "joinroom"
    n <- L.decimal
    return $ JoinRoom n

parseErrorMsg :: T.Text -> UserMessage
parseErrorMsg err = UserMessage (-1) $ ParseError err

