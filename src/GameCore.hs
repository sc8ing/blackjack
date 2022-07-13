{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleContexts #-}
module GameCore where

import Control.Monad.Random
import Control.Monad.State
import Data.Char (toLower, isSpace)
import Data.Default
import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.Random.Shuffle

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
  deriving (Show, Eq)
instance Read Move where
    readsPrec _ s = case toLower <$> filter (not . isSpace) s of
        ['s']      -> [(Stand, "")]
        ['h']      -> [(Hit, "")]
        ['d']      -> [(Double, "")]
        ['s', 'p'] -> [(Split, "")]
        ['s', 'u'] -> [(Surrender, "")]
        _ -> error $ "Couldn't parse move from " <> s

data Rules = Rules { _shoeDecks :: Int
                   , _penetration :: Float -- percent of shoe
                   , _minBet :: Int
                   } deriving (Show)
instance Default Rules where
    def = Rules 6 0.8 5

type MoveChooser = GameState -> Card -> Hand -> Move
type BetChooser = GameState -> Float
type InsuranceChooser = GameState -> Hand -> Bool
data Strategy = Strategy { _chooseMove :: MoveChooser
                         , _chooseBet :: BetChooser
                         , _chooseInsurance :: InsuranceChooser
                         }
instance Default Strategy where
    def = Strategy { _chooseMove = const . const . const Stand
                   , _chooseBet = const 5
                   , _chooseInsurance = const . const False
                   }

data GameState = GameState { _cardsUnplayed :: [Card]
                           , _cardsPlayed :: [Card]
                           , _bankroll :: Float
                           , _rules :: Rules
                           , _playerStrategy :: Strategy
                           }

makeGameState :: MonadRandom m => Rules -> Strategy -> Float -> m GameState
makeGameState rules strategy bankroll = do
    initShoe  <- makeShoe (_shoeDecks rules) (_penetration rules)
    pure GameState { _cardsUnplayed = initShoe
                   , _cardsPlayed = []
                   , _bankroll = bankroll
                   , _rules = rules
                   , _playerStrategy = strategy }

-- | Shuffle the cards and put them back in the shoe, keeping the rest the same
resetGameStateShoe :: MonadRandom m => GameState -> m GameState
resetGameStateShoe state =
    makeGameState (_rules state) (_playerStrategy state) (_bankroll state)

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

-- | Hi-Lo count of cards played
runningCount :: [Card] -> Int
runningCount = sum . map (\c -> if c `elem` [Two .. Six]      then  1
                                else if c `elem` [Ten .. Ace] then -1
                                else                                0)

trueCount :: Int -> [Card] -> Float
trueCount shoeDecks cardsPlayed =
    let cardsInDeck    = 52
        numCardsPlayed = length cardsPlayed
        decksPlayed    = fromIntegral numCardsPlayed / cardsInDeck
        decksLeft      = fromIntegral shoeDecks - decksPlayed
    in
    fromIntegral (runningCount cardsPlayed) / decksLeft

trueCountFromGameState :: GameState -> Float
trueCountFromGameState state =
    trueCount (_shoeDecks . _rules $ state) (_cardsPlayed state)

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
