{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.Random
import System.Random.Shuffle
import Control.Monad.State

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

data Move = Stand
          | Hit
          | Double
          | Split
          | Surrender

data Rules = Rules
  { _shoeDecks :: Int
  , _penetration :: Float -- percent of shoe
  , _minBet :: Int
  } deriving (Show)

type MoveChooser = GameState -> Card -> Hand -> Move
type BetChooser = GameState -> Float
type InsuranceChooser = GameState -> Hand -> Bool
data Strategy = Strategy
  { _chooseMove :: MoveChooser
  , _chooseBet :: BetChooser
  , _chooseInsurance :: InsuranceChooser
  }

data GameState = GameState
  { _cardsUnplayed :: [Card]
  , _cardsPlayed :: [Card]
  , _bankroll :: Float
  , _rules :: Rules
  , _playerStrategy :: Strategy
  }

----------------------------------------------------------------------------------------------------
-- calculation helpers
----------------------------------------------------------------------------------------------------
cardSum :: Hand -> HandScore
cardSum (Ace : rest) = Soft 11 <> cardSum rest
cardSum (card : rest) =
  let cardVals = Map.fromList $ zip [Two .. King] ([2..10] <> [10, 10, 10]) in
  Hard (cardVals ! card) <> cardSum rest
cardSum [] = Hard 0

handScoreInt :: HandScore -> Int
handScoreInt (Soft n) = n
handScoreInt (Hard n) = n

-- Hi-Lo count of cards played
runningCount :: [Card] -> Int
runningCount = sum . map (\c -> if c `elem` [Two .. Six]      then  1
                                else if c `elem` [Ten .. Ace] then -1
                                else                                0)

----------------------------------------------------------------------------------------------------
-- setup
----------------------------------------------------------------------------------------------------
makeShoe :: (MonadRandom m) => Int -> Float -> m [Card]
makeShoe decks penetration = do
    initDecks <- shuffleM $ [Two .. Ace] >>= replicate 4 >>= replicate decks
    let (shoe1, shoe2) = splitAt (floor (penetration * fromIntegral (length initDecks))) initDecks
    pure $ shoe1 <> [Yellow] <> shoe2

----------------------------------------------------------------------------------------------------
-- using cards
----------------------------------------------------------------------------------------------------
draw2 :: MonadState GameState m => m (Card, Card)
draw2 = drawShownCard >>= \c1 -> drawShownCard >>= \c2 -> pure (c1, c2)

drawHiddenCard :: MonadState GameState m => m Card
drawHiddenCard = drawCard True

drawShownCard :: MonadState GameState m => m Card
drawShownCard = drawCard False

drawCard :: MonadState GameState m => Bool -> m Card
drawCard silent = do
    state <- get
    -- shoe' should never be empty because we don't do full penetration
    let (card : shoe') = _cardsUnplayed state
    put state { _cardsUnplayed = shoe' }
    state <- get
    if card == Yellow
    then put state { _cardsPlayed = card : _cardsPlayed state } >> drawCard silent
    else if silent
         then pure card
         else put state { _cardsPlayed = card : _cardsPlayed state } >> pure card
