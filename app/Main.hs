{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Redundant <$>" #-}
module Main where

import Control.Monad.Random
import CsvLoading (loadChooseMoveFromCsv)
import Data.Default
import Data.IORef
import Debug.Trace
import GameCore
import GameExecution (playShoes)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import Text.Read (readMaybe, readEither)

main :: IO ()
main = do
    putStrLn "loading initial strategy"
    initRules <- askForRules
    initStrat <- askForStrat
    initBank  <- promptDefault 1000 "Starting bankroll: "
    initState <- makeGameState initRules initStrat initBank
    stateRef  <- newIORef initState
    repl stateRef

makeGameState :: MonadRandom m => Rules -> Strategy -> Float -> m GameState
makeGameState rules strategy bankroll = do
    initShoe  <- makeShoe (_shoeDecks rules) (_penetration rules)
    pure GameState { _cardsUnplayed = initShoe
                   , _cardsPlayed = []
                   , _bankroll = bankroll
                   , _rules = rules
                   , _playerStrategy = strategy }

resetGameState :: MonadRandom m => GameState -> m GameState
resetGameState state =
    makeGameState (_rules state) (_playerStrategy state) (_bankroll state)

repl :: IORef GameState -> IO ()
repl stateRef = (words <$> getLine) >>= \case
    ["play", n] -> case (readMaybe n :: Maybe Int) of
        Nothing ->
            putStrLn "play" >> repl stateRef
        Just n -> do
            state <- readIORef stateRef
            let result = playShoes state n
            print result
            state' <- resetGameState state
            writeIORef stateRef state'
            repl stateRef
    ["set", "bankroll", n] ->
        case (readMaybe n :: Maybe Float) of
          Nothing ->
            putStrLn "set bankroll [dollars]" >> repl stateRef
          Just n -> do
            modifyIORef stateRef (\s -> s { _bankroll = n }) >> repl stateRef
    "load" : toLoad ->
        let modifyStrat f =
              readIORef stateRef >>= (\curState ->
                -- guess I should get lenses for this
                modifyIORef stateRef (\s -> s { _playerStrategy = f (_playerStrategy curState) })
              )
        in
        (case toLoad of
            ["move"] -> do
                move <- askForMoveChooser
                modifyStrat (\s -> s { _chooseMove = move })
            ["bet"] -> do
                bet <- askForBetChooser []
                modifyStrat (\s -> s { _chooseBet = bet })
            ["insurance"] -> do
                ins <- askForInsuranceChooser
                modifyStrat (\s -> s { _chooseInsurance = ins })
            [] -> do
                strat <- askForStrat
                modifyStrat (const strat)
            _ ->
                putStrLn "can load move, bet, or insurance") >> repl stateRef
    h : _ | h `elem` ["q", "quit"]  ->
        putStrLn "bye, keep gambling!"
    _ ->
        putStrLn "not an understood command" >> listCommands >> repl stateRef

listCommands :: IO ()
listCommands =
    putStrLn "commands are play [shoes], set bankroll [size], load [move/bet/insurance]"

prompt :: Read a => String -> IO a
prompt toSay = do
    hSetBuffering stdout NoBuffering
    putStr toSay
    input <- getLine
    case readEither input of
       Right a  -> pure a
       Left err -> putStrLn err >> prompt toSay

promptDefault :: (Show a, Read a) => a -> String -> IO a
promptDefault def toSay =
    hSetBuffering stdout NoBuffering  >>
    putStr (toSay <> " (default " <> show def <> ") ") >>
    getLine >>= \case
        "" -> putStrLn "using default" >> pure def
        o -> case readEither o of
                Right a  -> pure a
                Left err -> putStrLn err >> promptDefault def toSay

-- Gross but idk how to make it not require "" when `Read`ing a string
promptDefaultString :: String -> String -> IO String
promptDefaultString def toSay =
    hSetBuffering stdout NoBuffering  >>
    putStr (toSay <> " (default " <> def <> ") ") >>
    getLine >>= \case
       "" -> putStrLn "using default" >> pure def
       i  -> pure i

askForRules :: IO Rules
askForRules =
    let Rules decks pen min = def in
    Rules <$> promptDefault decks "How many decks?"
          <*> promptDefault pen "Percent penetration: "
          <*> promptDefault min "Min bet: "

askForStrat :: IO Strategy
askForStrat =
    Strategy <$> askForMoveChooser <*> askForBetChooser [] <*> askForInsuranceChooser

askForMoveChooser :: IO MoveChooser
askForMoveChooser =
    putStrLn "give four arguments for hard, soft, split, & surrender charts" >>
    (words <$> getLine) >>= \case
    [hard, soft, split, surrender] ->
        loadChooseMoveFromCsv hard soft split surrender
    _ ->
        askForMoveChooser

getThreshBet :: IO (Float, Float)
getThreshBet = do
    thresh <- prompt "count to change bet at: " :: IO Float
    amount <- prompt "amount to bet at that count or higher: " :: IO Float
    pure (thresh, amount)

askForBetChooser :: [(Float, Float)] -> IO BetChooser
askForBetChooser thresholds = promptDefaultString "no" "add bet strategy? (y/n)" >>= \case
    "n" ->
        pure (\state ->
            let count = trueCountFromGameState state
                minBet = fromIntegral $ (_minBet . _rules) state
                takeClosestThresh =  (\(nextThres, nextBet) (curThresh, curBet) ->
                    if nextThres > curThresh && count >= nextThres then (nextThres, nextBet)
                                                                   else (curThresh, curBet))
                (thresh, bet) = foldr takeClosestThresh (-10000, minBet) thresholds
            in
            bet
        )
    "y" ->
        getThreshBet >>= \tb -> askForBetChooser (tb : thresholds)
    _ ->
        putStrLn "what" >> askForBetChooser thresholds

askForInsuranceChooser :: IO InsuranceChooser
askForInsuranceChooser = do
    thresh <- promptDefault 3 "at what count or above would you take insurance at?" :: IO Float
    pure (\state hand -> trueCountFromGameState state >= thresh)
