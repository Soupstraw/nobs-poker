module Hands 
  ( deck52, deck24
  , handStrength
  , allHands
  ) where

import Relude

import Data.List as L
import Data.Maybe

import qualified Text.Show

--import Debug.Trace

data PokerHand
  = HighCard Card
  | OnePair (Card, Card)
  | TwoPair (Card, Card) (Card, Card)
  | ThreeOfAKind (Card, Card, Card)
  | Straight (Card, Card, Card, Card, Card)
  | FullHouse (Card, Card, Card) (Card, Card)
  | Flush (Card, Card, Card, Card, Card)
  | FourOfAKind (Card, Card, Card, Card)
  | StraightFlush (Card, Card, Card, Card, Card)
  | DoubleStraightFlush 
      (Card, Card, Card, Card, Card) 
      (Card, Card, Card, Card, Card)
  | TripleStraightFlush 
      (Card, Card, Card, Card, Card) 
      (Card, Card, Card, Card, Card)
      (Card, Card, Card, Card, Card)
  | QuadStraightFlush 
      (Card, Card, Card, Card, Card) 
      (Card, Card, Card, Card, Card) 
      (Card, Card, Card, Card, Card) 
      (Card, Card, Card, Card, Card)
  deriving (Show, Eq, Ord)

follows :: Card -> Card -> Bool
follows x y
  | _cRank x == R2 && _cRank y == RA = True
  | _cRank y == RA = False
  | otherwise = _cRank x == succ (_cRank y)

handStrength :: [Card] -> PokerHand
handStrength hand = fromJust . asum $ handStrengths hand

allHands :: [Card] -> [PokerHand]
allHands = catMaybes . handStrengths

handStrengths :: [Card] -> [Maybe PokerHand]
handStrengths hand =
  [ quadStraightFlush sorted
  , tripleStraightFlush sorted
  , doubleStraightFlush sorted
  , straightFlush sorted
  , fourOfAKind sorted
  , flush sorted
  , straight sorted
  , threeOfAKind sorted
  , fullHouse sorted
  , twoPair sorted
  , onePair sorted
  , highCard hand
  ]
  where
    sorted = reverse $ sort hand
    highCard = Just . HighCard . L.head
    fullHouse :: [Card] -> Maybe PokerHand
    fullHouse h =
      do
        let ranked = groupBy (\x y -> _cRank x == _cRank y) h
        xIdx <- findIndex ((>=3) . length) ranked
        let (l, r') = splitAt xIdx ranked
        let r = L.tail r'
        yIdx <- findIndex ((>=2) . length) $ l <> r
        let x1:x2:x3:_ = ranked !! xIdx
        let y1:y2:_    = (l <> r) !! yIdx
        return $ FullHouse (x1, x2, x3) (y1, y2)
    takeFlushes n cards =
      do
        let suited = groupBy (\x y -> _cSuit x == _cSuit y) $ sortOn _cSuit cards
        let straightsBySuit = fmap (\(Straight x) -> x) . catMaybes $ straight <$> suited
        if length straightsBySuit < n
          then Nothing
          else Just . take n $ sort straightsBySuit
    quadStraightFlush :: [Card] -> Maybe PokerHand
    quadStraightFlush h =
      do
        x1:x2:x3:x4:_ <- takeFlushes 4 h
        return $ QuadStraightFlush x1 x2 x3 x4
    tripleStraightFlush :: [Card] -> Maybe PokerHand
    tripleStraightFlush h =
      do
        x1:x2:x3:_ <- takeFlushes 3 h
        return $ TripleStraightFlush x1 x2 x3
    doubleStraightFlush :: [Card] -> Maybe PokerHand
    doubleStraightFlush h =
      do
        x1:x2:_ <- takeFlushes 2 h
        return $ DoubleStraightFlush x1 x2
    straightFlush :: [Card] -> Maybe PokerHand
    straightFlush h =
      do
        x1:_ <- takeFlushes 1 h
        return $ StraightFlush x1
    flush :: [Card] -> Maybe PokerHand
    flush h = 
      do
        let suited = groupBy (\x y -> _cSuit x == _cSuit y) $ sortOn _cSuit h
        x1:x2:x3:x4:x5:_ <- find ((>= 5) . length) suited
        return $ Flush (x1, x2, x3, x4, x5)
    findRepeats :: Int -> [Card] -> Maybe [Card]
    findRepeats n h =
      do
        let ranked = groupBy (\x y -> _cRank x == _cRank y) h
        find ((>=n) . length) ranked
      
    fourOfAKind :: [Card] -> Maybe PokerHand
    fourOfAKind h =
      do
        x1:x2:x3:x4:_ <- findRepeats 4 h
        return $ FourOfAKind (x1, x2, x3, x4)

    threeOfAKind :: [Card] -> Maybe PokerHand
    threeOfAKind h =
      do
        x1:x2:x3:_ <- findRepeats 3 h
        return $ ThreeOfAKind (x1, x2, x3)
    onePair :: [Card] -> Maybe PokerHand
    onePair h =
      do
        x1:x2:_ <- findRepeats 2 h
        return $ OnePair (x1, x2)
    twoPair :: [Card] -> Maybe PokerHand
    twoPair h =
      do
        let ranked = groupBy (\x y -> _cRank x == _cRank y) h
        let idxs = findIndices ((>=2) . length) ranked
        guard $ length idxs >= 2
        let x:y:_ = idxs
        let p1 = ranked !! x
        let p2 = ranked !! y
        return $ TwoPair (L.head p1, L.last p1) (L.head p2, L.last p2)
    straight :: [Card] -> Maybe PokerHand
    straight hand' = 
      do
        let aces = filter ((==RA) . _cRank) hand'
        let h = aces <> hand'
        let checkStraight (x1:x2:x3:x4:x5:t)
              | x2 `follows` x1 &&
                x3 `follows` x2 &&
                x4 `follows` x3 &&
                x5 `follows` x4 = Just $ Straight (x1, x2, x3, x4, x5)
              | otherwise = checkStraight $ x2:x3:x4:x5:t
            checkStraight _ = Nothing
        --traceShowM $ sortOn (Down . _cRank) h
        --traceShowM $ reverse h
        checkStraight . reverse . map L.head $ groupBy (\x y -> _cRank x == _cRank y) h

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

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Enum, Eq, Ord)

instance Show Suit where
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"
  show Spades   = "♠"

data Card = Card 
  { _cRank :: Rank 
  , _cSuit :: Suit
  } deriving (Eq, Ord)

instance Show Card where
  show c = show (_cRank c) <> show (_cSuit c)

instance Enum Card where
  fromEnum (Card r s) = fromEnum r * 4 + fromEnum s
  toEnum x = Card (toEnum $ x `div` 4) (toEnum $ x `mod` 4)

deck24 :: [Card]
deck24 = drop 28 deck52

deck52 :: [Card]
deck52 = Card <$> [R2 ..] <*> [Clubs ..]

