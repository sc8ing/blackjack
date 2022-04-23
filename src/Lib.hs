{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib where

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Card = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          | Yellow
  deriving (Show, Eq, Ord, Enum)

type Hand = [Card]

data HandScore = Soft Int | Hard Int deriving (Show)
instance Semigroup HandScore where
  Hard x <> Hard y = Hard (x + y)
  Hard x <> Soft y = if x + y > 21 then Hard (x + y - 10) else Soft (x + y)
  Soft x <> Soft y = if x + y <= 21 then Soft (x + y) else Soft x <> Hard (y - 10)
  x <> y           = y <> x

cardSum :: Hand -> HandScore
cardSum (Ace : rest) = Soft 11 <> cardSum rest
cardSum (card : rest) =
  let cardVals = Map.fromList $ zip [Two .. King] ([2..10] <> [10, 10, 10]) in
  Hard (cardVals ! card) <> cardSum rest
cardSum [] = Hard 0

handScoreInt :: HandScore -> Int
handScoreInt (Soft n) = n
handScoreInt (Hard n) = n

data Move = Stand
    | Hit
    | Double
    | Split
    | Surrender

data Settings = Settings
  { _shoeDecks :: Int
  , _penetration :: Float -- percent of shoe
  , _minBet :: Int
  } deriving (Show)

data GameState = GameState
  { _cardsUnplayed :: [Card]
  , _cardsPlayed :: [Card]
  , _bankroll :: Float
  , _settings :: Settings
  } deriving (Show)
