{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- Todo:
--  - load strategy from CSV
--  - multiple players

import Lib
import Control.Monad.State
import Control.Monad.Random
import qualified System.Random.Shuffle
import System.Random.Shuffle (shuffleM)
import Data.Foldable (traverse_)
import Debug.Trace

main :: IO ()
main = playShoes 50 >>= print

playShoes n = (/ n) . sum <$> traverse (const (_bankroll <$> runPlayNewShoe)) [1..n]

makeShoe :: (MonadRandom m) => Int -> Float -> m [Card]
makeShoe decks penetration = do
    initDecks <- shuffleM $ [Two .. Ace] >>= replicate 4 >>= replicate decks
    let (shoe1, shoe2) = splitAt (floor (penetration * fromIntegral (length initDecks))) initDecks
    pure $ shoe1 <> [Yellow] <> shoe2

runPlayNewShoe :: MonadRandom m => m GameState
runPlayNewShoe = do
      let settings = Settings { _shoeDecks = 6
                              , _penetration = 0.8
                              , _minBet = 5 }
      shoe <- makeShoe (_shoeDecks settings) (_penetration settings)
      let gameState = GameState { _cardsUnplayed = shoe
                                , _cardsPlayed = []
                                , _bankroll = 1000
                                , _settings = settings
                                }
          (_, finalState) = runState playShoe gameState
      pure finalState

playShoe :: (MonadState GameState m) => m ()
playShoe = do
    bet         <- chooseBet <$> get
    if bet <= 0 then error "negative bet!" else pure ()
    adjustBankroll (-bet)

    dealerUp    <- drawShownCard
    dealerDown  <- drawHiddenCard
    hand        <- fmap (\(c1, c2) -> [c1, c2]) draw2

    state <- get
    hasInsurance <- if dealerUp == Ace && chooseInsurance state hand
                    then adjustBankroll (bet / 2) >> pure True
                    else pure False

    let dealerBj = (handScoreInt . cardSum) [dealerUp, dealerDown] == 21
        playerBj = (handScoreInt . cardSum) hand == 21
    if dealerBj && playerBj
    then if hasInsurance
         -- gets bet back from push, plus 2:1 pay on insurance bet
         then adjustBankroll (bet + (bet / 2) * 3) >> pure ()
         -- just give the bet back cause it was a push
         else adjustBankroll bet >> pure ()
    -- skip playing out the hand and go to yellow card checking
    -- TODO do you still play out the hand?
    else if dealerBj && not playerBj then pure ()

    -- get bet back + payout of 3:2 & skip playing rest of hand
    else if not dealerBj && playerBj then adjustBankroll (bet + bet * 3 / 2) >> pure ()
    -- both not blackjack
    else playOutHand hand bet dealerUp dealerDown >> pure ()

    cardsPlayed <- gets _cardsPlayed
    if Yellow `elem` cardsPlayed then pure () -- end of shoe
    else playShoe

-- playHand might end up playing multiple hands if splits happen!
-- so it returns a list of hands & their bets waiting to go against the
-- dealer's hand. In the case of surrendering, busting or blackjack,
-- it gives an empty list because there's nothing for the dealer to do.
playOutHand :: MonadState GameState m => Hand -> Float -> Card -> Card -> m ()
playOutHand hand bet dealerUp dealerDown =
    let playHand curHand curBet = case cardSum curHand of
            Hard n | n > 21 ->
                pure []
            Soft n | n > 21 ->
                pure []
            Soft n | n == 21 ->
                if length curHand == 2 then error "should have already checked for blackjack" else
                pure [(curHand, curBet)]
            Hard n | n == 21 ->
                if length curHand == 2 then error "should have already checked for blackjack" else
                pure [(curHand, curBet)]
            -- If we don't have blackjack & haven't busted, decide what to do
            _ -> get >>= \state -> case chooseMove state dealerUp curHand of
                Stand ->
                  pure [(curHand, curBet)]
                Hit -> do
                  c <- drawShownCard
                  playHand (c : curHand) curBet
                Double -> do
                  c <- drawShownCard
                  adjustBankroll (- curBet)
                  pure [(c : curHand, curBet * 2)]
                Split ->
                  case curHand of
                    [c1, c2] | c1 == c2 -> do
                      (c1', c2') <- draw2
                      adjustBankroll (- curBet)
                      hands1 <- playHand [c1, c1'] curBet
                      hands2 <- playHand [c2, c2'] curBet
                      pure (hands1 <> hands2)
                    _ -> error "can't split this"
                Surrender -> do
                  adjustBankroll (curBet / 2)
                  pure []
    in do
        -- these are awaiting evaluation against the dealer
        handBets <- playHand hand bet

        -- add the hidden dealer card to the cards played list so it's
        -- accurate when next calling chooseMove/Bet/Insurance functions
        state <- get
        put state { _cardsPlayed = dealerDown : _cardsPlayed state }

        let playDealer cards = case cardSum cards of
                Hard n | n >= 17 -> pure cards
                Soft n | n > 17  -> pure cards
                _                -> drawShownCard >>= playDealer . (: cards)
        dealerSum <- handScoreInt . cardSum <$> playDealer [dealerUp, dealerDown]

        let doHandVsDealer (hand, bet) =
                let handSum = (handScoreInt . cardSum) hand
                in
                if      dealerSum > handSum then adjustBankroll 0
                else if dealerSum < handSum then adjustBankroll (bet * 2)
                else                             adjustBankroll bet
        traverse_ doHandVsDealer handBets

-- drawN :: Int -> State GameState Hand
-- drawN n = traverse (const drawShownCard) [1..n]
draw2 :: MonadState GameState m => m (Card, Card)
draw2 = drawShownCard >>= \c1 -> drawShownCard >>= \c2 -> pure (c1, c2)

adjustBankroll :: MonadState GameState m => Float -> m Float
adjustBankroll amount = do
  s <- get
  let new = _bankroll s + amount
  put s { _bankroll = new }
  pure new

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

chooseMove :: GameState -> Card -> Hand -> Move
chooseMove _ dealerUp playerHand =
  case cardSum playerHand of
    Hard n | n >= 17 -> Stand
    Hard n | (12 <= n && n <= 16) && (Two <= dealerUp && dealerUp <= Six) -> Surrender
    Hard 11          -> Double
    _ -> Hit

chooseBet :: GameState -> Float
chooseBet state = max 25 (trueCount - 1) * 25 where
    trueCount   = fromIntegral (runningCount played) / decksLeft
    played      = _cardsPlayed state
    decksLeft   = fromIntegral (_shoeDecks . _settings $ state) - decksPlayed
    decksPlayed = cardsPlayed / cardsInDeck
    cardsPlayed = fromIntegral (length played) :: Float
    cardsInDeck = 52

-- Hi-Lo count of cards played
runningCount :: [Card] -> Int
runningCount = sum . map (\c -> if c `elem` [Two .. Six]      then  1
                                else if c `elem` [Ten .. Ace] then -1
                                else                                0)

chooseInsurance :: GameState -> Hand -> Bool
chooseInsurance _ _ = False
