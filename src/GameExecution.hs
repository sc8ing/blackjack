{-# LANGUAGE FlexibleContexts #-}
module GameExecution
  ( playShoes,
  )
where

import Control.Monad.Random
import Control.Monad.State
import CsvLoading (loadChooseMoveFromCsv)
import Data.Char (isNumber)
import Data.Foldable (traverse_)
import Data.List.Split (splitOn)
import Debug.Trace
import GameCore
import qualified System.Random.Shuffle
import System.Random.Shuffle (shuffleM)
import Text.CSV

playShoes :: (MonadIO m, MonadRandom m, MonadFail m) => Int -> m Float
playShoes n = do
    moveChooser <- loadChooseMoveFromCsv "strategies/test/hard.csv" "strategies/test/soft.csv" "strategies/test/split.csv" "strategies/test/surrender.csv"
    let betChooser = const (5 :: Float)
        insuranceChooser = const. const False
    let rules = Rules { _shoeDecks = 6
                      , _penetration = 0.8
                      , _minBet = 5 }
    shoe <- makeShoe (_shoeDecks rules) (_penetration rules)
    let initState = GameState { _cardsUnplayed = shoe
                              , _cardsPlayed = []
                              , _bankroll = 1000
                              , _rules = rules
                              , _playerStrategy = Strategy moveChooser betChooser insuranceChooser }
    endBankrolls <- traverse (const (_bankroll <$> runPlayNewShoe initState)) [1..n]
    pure $ sum endBankrolls / fromIntegral n

runPlayNewShoe :: MonadRandom m => GameState -> m GameState
runPlayNewShoe initState =
    let (_, finalState) = runState playShoe initState in
    pure finalState

playShoe :: (MonadState GameState m) => m ()
playShoe = do
    bet         <- gets (_chooseBet . _playerStrategy) <*> get
    if bet <= 0 then error "negative bet!" else pure ()
    adjustBankroll (-bet)

    dealerUp    <- drawShownCard
    dealerDown  <- drawHiddenCard
    hand        <- fmap (\(c1, c2) -> [c1, c2]) draw2

    state <- get
    chooseInsurance <- gets (_chooseInsurance . _playerStrategy)
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
                -- TODO blackjack or just 21?
                -- if length curHand == 2 then Stand else
                pure [(curHand, curBet)]
            Hard n | n == 21 ->
                -- TODO blackjack?
                -- if length curHand == 2 then error "should have already checked for blackjack" else
                pure [(curHand, curBet)]
            -- If we don't have blackjack & haven't busted, decide what to do
            _ -> get >>= \state -> case (_chooseMove . _playerStrategy) state state dealerUp curHand of
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

adjustBankroll :: MonadState GameState m => Float -> m Float
adjustBankroll amount = do
  s <- get
  let new = _bankroll s + amount
  put s { _bankroll = new }
  pure new
