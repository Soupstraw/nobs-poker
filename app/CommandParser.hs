module CommandParser 
  ( parseMessage
  , Command(..)
  ) where

import Relude

import Data.Attoparsec.Text as P
import qualified Data.Text as T

data Command 
  = Say Text
  | Raise Int
  | Call
  | Fold
  | Join Word
  | Error Text

parseMessage :: Text -> (Word, Command)
parseMessage msg = either f id $ parseOnly messageParser msg
  where f x = (toEnum 0, Error $ toText x)

messageParser :: Parser (Word, Command)
messageParser = 
  do
    roomId <- lexeme decimal
    cmd    <- parseSay <|> parseCommand
    return (roomId, cmd)

parseSay :: Parser Command
parseSay = 
  do
    h <- notChar '/'
    rest <- takeText
    endOfInput
    return . Say $ T.cons h rest

parseCommand :: Parser Command
parseCommand =
  do
    void $ char '/'
    res <- choice
      [ Raise <$> (symbol "raise" *> lexeme decimal)
      , Join  <$> (symbol "join"  *> lexeme decimal)
      , symbol "call" $> Call
      , symbol "fold" $> Fold
      ]
    endOfInput
    return res

lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

symbol :: Text -> Parser Text
symbol s = string s <* skipSpace

