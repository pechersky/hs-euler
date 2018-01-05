{-# LANGUAGE LambdaCase #-}

module Euler.P050.Problem054
  ( prob054
  )
  where

{- In the card game poker, a hand consists of five cards and are ranked,
 - from lowest to highest, in the following way:
 -
 - High Card: Highest value card.
 - One Pair: Two cards of the same value.
 - Two Pairs: Two different pairs.
 - Three of a Kind: Three cards of the same value.
 - Straight: All cards are consecutive values.
 - Flush: All cards of the same suit.
 - Full House: Three of a kind and a pair.
 - Four of a Kind: Four cards of the same value.
 - Straight Flush: All cards are consecutive values of same suit.  - Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
 - The cards are valued in the order:
 - 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
 -
 - If two players have the same ranked hands then the rank made up of
 - the highest value wins;
 - for example, a pair of eights beats a pair of fives (see example 1 below).
 - But if two ranks tie, for example, both players have a pair of queens,
 - then highest cards in each hand are compared (see example 4 below);
 - if the highest cards tie then the next highest cards are compared, and so on.
 -
 - Consider the following five hands dealt to two players:
 -
 - Hand      Player 1        Player 2        Winner
 - 1         5H 5C 6S 7S KD  2C 3S 8S 8D TD  Player 2
 -           Pair of Fives   Pair of Eights
 - 2         5D 8C 9S JS AC  2C 5C 7D 8S QH  Player 1
 -         Highest card Ace  Highest card Queen
 - 3         2D 9C AS AH AC  3D 6D 7D TD QD  Player 2
 -           Three Aces      Flush with Diamonds
 - 4         4D 6S 9H QH QC  3D 6D 7H QD QS  Player 1
 -           Pair of Queens  Pair of Queens
 -        Highest card Nine  Highest card Seven
 - 5         2H 2D 4C 4D 4S  3C 3D 3S 9S 9D  Player 1
 -           Full House      Full House
 -         With Three Fours  with Three Threes
 - The file, poker.txt, contains one-thousand random hands dealt to two players.
 - Each line of the file contains ten cards (separated by a single space):
 - the first five are Player 1's cards and the last five are Player 2's cards.
 - You can assume that all hands are valid (no invalid characters or repeated cards),
 - each player's hand is in no specific order,
 - and in each hand there is a clear winner.
 -
 - How many hands does Player 1 win? -}

import Data.Ord             (compare)
import Data.Function        (on)
import Data.Void            (Void)
import Data.List            (sort, groupBy)
import Data.Either          (either)
import Text.Megaparsec      (runParser, Parsec, sepEndBy1, choice)
import Text.Megaparsec.Char (char, newline, space)

prob054 :: IO Integer
prob054 = do
  input <- readFile txtfile
  let games = either (error "broken parse") id $ runParser (parseGame `sepEndBy1` newline) txtfile input
  return $ prob054' games

-- naive method

txtfile :: String
txtfile = "src/Euler/P050/p054_poker.txt"

prob054' :: [(Hand, Hand)] -> Integer
prob054' = foldr (\case GT -> succ; _ -> id) 0 . fmap (uncurry (compare `on` score))

parseGame :: Parser (Hand, Hand)
parseGame = (,) <$> parseHand <* space <*> parseHand

type Parser = Parsec Void String

data Value = V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | VT | VJ | VQ | VK | VA
  deriving (Show, Enum, Bounded, Eq, Ord)

parseValue :: Parser Value
parseValue = choice $ zipWith (\v c -> v <$ char c) [minBound .. maxBound] "23456789TJQKA"

data Suit = C | D | H | S
  deriving (Show, Enum, Bounded, Eq, Ord)

parseSuit :: Parser Suit
parseSuit = choice $ zipWith (\v c -> v <$ char c) [minBound .. maxBound] "CDHS"

data Card = Card {value :: Value, suit :: Suit}
  deriving (Show)

parseCard :: Parser Card
parseCard = Card <$> parseValue <*> parseSuit

instance Eq Card where
  (==) = (==) `on` value

instance Ord Card where
  compare = compare `on` value

type Hand = [Card]

parseHand :: Parser Hand
parseHand = sequence (replicate 4 (parseCard <* space) ++ [parseCard])

data Score = High | OnePair | TwoPair | ThreeKind | Straight | Flush | FullHouse | FourKind | StraightFlush | RoyalFlush
  deriving (Show, Enum, Bounded, Eq, Ord)

type FullScore = (Score, Card, Card)

score :: Hand -> FullScore
score (sort->hand) = (score', hitCard score', maxCard)
  where
    score' :: Score
    score'
      | flush hand && straight hand && value maxCard == VA = RoyalFlush
      | flush hand && straight hand                        = StraightFlush
      | vmatch 4 hand                                      = FourKind
      | vmatch 3 hand && vmatch 2 hand                     = FullHouse
      | flush hand                                         = Flush
      | straight hand                                      = Straight
      | vmatch 3 hand                                      = ThreeKind
      | vmatch 2 hand && length (g value hand) == 3        = TwoPair
      | vmatch 2 hand                                      = OnePair
      | otherwise                                          = High
    hitCard :: Score -> Card
    hitCard = \case
      FourKind  -> last . last . filter ((== 4) . length) . g value $ hand
      FullHouse -> last . last . filter ((== 3) . length) . g value $ hand
      ThreeKind -> last . last . filter ((== 3) . length) . g value $ hand
      TwoPair   -> last . last . filter ((== 2) . length) . g value $ hand
      OnePair   -> last . last . filter ((== 2) . length) . g value $ hand
      _         -> maxCard
    maxCard = last hand
    g f = groupBy ((==) `on` f)
    flush = (== 5) . maximum . fmap length . g suit
    vmatch n = elem n . fmap length . g value
    straight = matchUp

matchUp :: [Card] -> Bool
matchUp xs = matchUp' xs xs
  where
    matchUp' (value . head -> val) = and . zipWith (==) (enumFrom val) . fmap value
