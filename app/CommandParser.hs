{-# LANGUAGE OverloadedStrings #-}

module CommandParser 
  (
  ) where

import Control.Lens

import Data.Foldable
import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

data UserMessage = UserMessage
  { _umAction :: UserAction
  }

data UserAction 
  = SetName T.Text
  | JoinRoom Int
  | Say T.Text
  | ParseError T.Text

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseMessage :: T.Text -> UserMessage
parseMessage msg = case parse messageParser "" msg of
  Left e  -> UserMessage . ParseError . T.pack $ errorBundlePretty e
  Right x -> x

messageParser :: Parser UserMessage
messageParser = 
  do
    try $ char '/'
    act <- asum 
      [ try $ setNameParser
      , joinRoomParser
      ]
    return . UserMessage $ act
  <|> (UserMessage . Say <$> takeRest)

setNameParser :: Parser UserAction
setNameParser =
  do
    symbol "setname"
    n <- lexeme $ count' 3 2 asciiChar
    eof
    return . SetName . T.pack $  n

joinRoomParser :: Parser UserAction
joinRoomParser =
  do
    symbol "joinroom"
    n <- L.decimal
    return $ JoinRoom n

