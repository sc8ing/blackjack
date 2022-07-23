{-# LANGUAGE FlexibleContexts #-}
module GameExecution
  ( playShoes,
  )
where

import Control.Monad.Random
import Control.Monad.State
import CsvLoading (loadChooseMoveFromCsv)
import Data.Char (isNumber)
import Data.Foldable (traverse_, foldlM)
import Data.List.Split (splitOn)
import Debug.Trace
import GameCore
import qualified System.Random.Shuffle
import System.Random.Shuffle (shuffleM)
import Text.CSV

-- | Play n shoes sequentially with the end state of each feeding into the next.
-- | Returns the final bankroll player's left with.
playShoes :: MonadRandom m => GameState -> Int -> m Float
playShoes initState n = do
    endState <- foldlM (\state' _ -> fmap runPlayShoe (resetGameStateShoe state'))
                       initState
                       [1..n]
    pure $ _bankroll endState

runPlayShoe :: GameState -> GameState
runPlayShoe initState =
    let (_, finalState) = runState playShoe initState in
    finalState

-- | Keep playing hands until the yellow card's been used
playShoe :: (MonadState GameState m) => m ()
playShoe = do
    playHand
    cardsPlayed <- gets _cardsPlayed
    if Yellow `elem` cardsPlayed then pure () -- end of shoe
    else playShoe

playHand :: MonadState GameState m => m ()
playHand = do
    bet         <- gets (_chooseBet . _playerStrategy) <*> get
    if bet <= 0 then error "negative bet!" else pure ()

    dealerUp    <- drawShownCard
    dealerDown  <- drawHiddenCard
    playerHand  <- fmap (\(c1, c2) -> [c1, c2]) draw2

    insChooser  <- gets (_chooseInsurance . _playerStrategy)
    insState    <- get
    let tookIns = dealerUp == Ace && insChooser insState playerHand

    let dealerBj = handIntValue [dealerUp, dealerDown] == 21
        playerBj = handIntValue playerHand == 21
    case (dealerBj, playerBj) of
         -- 2:1 pay on insurance bet ((bet / 2) * 2) and push on the initial bet
        (True, True) | tookIns ->
            trace ("both blackjack & took insurance " <> show playerHand <> " vs " <> show [dealerUp, dealerDown] <> " -> won " <> show bet) adjustBankroll bet
        -- nothing paid on a blackjack push? TODO is this true?
        (True, True) ->
            trace ("both blackjack & no insurance " <> show playerHand <> " vs " <> show [dealerUp, dealerDown] <> " -> no money change ") pure ()
        -- lost to dealer blackjack
        (True, False) ->
            trace ("dealer blackjacked & not us -> lost " <> show playerHand <> " vs " <> show [dealerUp, dealerDown] <> " -> lost " <> show (-bet)) adjustBankroll (-bet)
        -- 3:2 pay on blackjack
        (False, True) ->
            trace ("we blackjacked & the dealer didn't! -> won " <> show playerHand <> " vs " <> show [dealerUp, dealerDown] <> " -> won " <> show (bet * 3 / 2)) adjustBankroll (bet * 3 / 2)
        -- normal play (no initial blackjacks, but could still get one if splitting aces)
        (False, False) -> do
            endPlayerHandBets <- playOutPlayerHand dealerUp dealerDown playerHand bet
            -- add the hidden dealer card to the cards played list so cardsPlayed is accurate next round
            state <- get
            put state { _cardsPlayed = dealerDown : _cardsPlayed state }
            -- only play out the dealer's hand if there's a non blackjack and unbusted player hand, cause that's the rules
            let isBlackjack hand = handIntValue hand == 21 && length hand == 2
                busted hand = handIntValue hand > 21
            endDealerHand <- if all (\h -> isBlackjack h || busted h) (fmap fst endPlayerHandBets)
                             then pure [dealerUp, dealerDown]
                             else playOutDealerHand dealerUp dealerDown
            let totalWon = sum $ fmap (uncurry (evaluateHand endDealerHand)) endPlayerHandBets
            trace ("final player hands: " <> show endPlayerHandBets
                  <> " final dealer hand: " <> show endDealerHand
                  <> " totalWon: " <> show totalWon) $ adjustBankroll totalWon

-- | Compare the player's hand to the dealer's and return how much the player won (negative for loss).
evaluateHand :: Hand -> Hand -> Float -> Float
evaluateHand dealerHand playerHand bet =
    let handSum = handIntValue playerHand
        dealerSum = handIntValue dealerHand
    in
    -- Doesn't matter if the dealer had blackjack because we wouldn't be playing out the hand if that were the case
    if handSum == 21 && length playerHand == 2        then  bet * 3 / 2 -- blackjack!
    else if handSum > 21 || handSum == 0              then -bet         -- we busted or surrendered, doesn't matter what dealer did -> lost
    else if (dealerSum > 21) || (dealerSum < handSum) then  bet         -- dealer busted or player was higher -> won
    else if dealerSum > handSum                       then -bet         -- no one busted, but player lost
    else                                                    0           -- push

playOutDealerHand :: MonadState GameState m => Card -> Card -> m Hand
playOutDealerHand dealerUp dealerDown = do
    let playDealer cards = case cardSum cards of
            Hard n | n >= 17 -> pure cards
            Soft n | n > 17  -> pure cards
            _                -> drawShownCard >>= playDealer . (: cards)
    playDealer [dealerUp, dealerDown]

-- | Play out the hand until all hands (i.e. after splits) have busted, stood, surrendered, doubled, or hit 21.
-- | In the case of surrendering, returns an empty hand with a bet. Otherwise, a tuple of hands and their bets are given.
playOutPlayerHand :: MonadState GameState m => Card -> Card -> Hand -> Float -> m [(Hand, Float)]
playOutPlayerHand dealerUp dealerDown hand bet = case cardSum hand of
     Hard n | n > 21 ->
         pure [(hand, bet)]
     Soft n | n > 21 ->
         pure [(hand, bet)]
     Soft n | n == 21 ->
         pure [(hand, bet)]
     Hard n | n == 21 ->
         pure [(hand, bet)]
     -- If we don't have 21 & haven't busted, decide what to do
     _ -> get >>= \state -> case (_chooseMove . _playerStrategy) state state dealerUp hand of
         Stand ->
             pure [(hand, bet)]
         Hit -> do
             c <- drawShownCard
             playOutPlayerHand dealerUp dealerDown (c : hand) bet
         -- can't double if not first move, so hit otherwise for now
         Double | length hand == 2 -> do
             c <- drawShownCard
             pure [(c : hand, bet * 2)]
         Double -> do
             c <- drawShownCard
             playOutPlayerHand dealerUp dealerDown (c : hand) bet
         Split ->
             case hand of
                 [c1, c2] | c1 == c2 -> do
                     (c1', c2') <- draw2
                     hands1 <- playOutPlayerHand dealerUp dealerDown [c1, c1'] bet
                     hands2 <- playOutPlayerHand dealerUp dealerDown [c2, c2'] bet
                     pure (hands1 <> hands2)
                 _ -> error $ "asked to split " <> show hand <> " but that's against the rules"
         Surrender ->
             pure [([], bet / 2)]

-- | Add an amount to the player's bankroll
adjustBankroll :: MonadState GameState m => Float -> m ()
adjustBankroll amount = do
  s <- get
  let new = _bankroll s + amount
  put s { _bankroll = new }
