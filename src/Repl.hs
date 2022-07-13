{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Repl
  ( makeRepl,
    repl,
  )
where

import Control.Monad.Random
import CsvLoading
import Data.Default
import Data.IORef
import Debug.Trace
import GameCore
import GameExecution (playShoes)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import Text.Read (readMaybe, readEither)

makeRepl :: IO ()
makeRepl = do
    putStrLn "loading initial strategy"
    initRules <- askForRules
    initStrat <- askForStrat
    initBank  <- promptDefault 1000 "Starting bankroll: "
    initState <- makeGameState initRules initStrat initBank
    stateRef  <- newIORef initState
    repl stateRef

-- TODO why am I using an IORef here instead of MonadReader/State?
repl :: IORef GameState -> IO ()
repl stateRef = putStrLn "ready" >> readIORef stateRef >>= \curState -> (words <$> getLine) >>= \case
    ["play", n] -> case (readMaybe n :: Maybe Int) of
        Nothing ->
            putStrLn "play" >> repl stateRef
        Just n -> do
            result <- playShoes curState n
            print result
            state' <- resetGameStateShoe curState
            writeIORef stateRef state'
            repl stateRef
    ["set", "bankroll", n] ->
        case (readMaybe n :: Maybe Float) of
            Nothing ->
                putStrLn "set bankroll [dollars]" >> repl stateRef
            Just n -> do
                modifyIORef stateRef (\s -> s { _bankroll = n }) >> repl stateRef
    "load" : toLoad ->
        -- guess lenses might be worth it at this point?
        let modifyStrat f = modifyIORef stateRef (\s -> s {
            _playerStrategy = f (_playerStrategy curState)
        })
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
        "" -> pure def
        o -> case readEither o of
                Right a  -> pure a
                Left err -> putStrLn err >> promptDefault def toSay

-- Gross but idk how to make it not require "" when `Read`ing a string
promptDefaultString :: String -> String -> IO String
promptDefaultString def toSay =
    hSetBuffering stdout NoBuffering  >>
    putStr (toSay <> " (default " <> def <> ") ") >>
    getLine >>= \case
       "" -> pure def
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
askForMoveChooser = do
    hard      <- promptDefaultString "strategies/basic/hard.csv" "hard chart: "
    soft      <- promptDefaultString "strategies/basic/soft.csv" "soft chart: "
    split     <- promptDefaultString "strategies/basic/split.csv" "split chart: "
    surrender <- promptDefaultString "strategies/basic/surrender.csv" "surrender chart: "
    loadChooseMoveFromCsv hard soft split surrender

getThreshBet :: IO (Float, Float)
getThreshBet = do
    thresh <- prompt "count to change bet at: " :: IO Float
    amount <- prompt "amount to bet at that count or higher: " :: IO Float
    pure (thresh, amount)

askForBetChooser :: [(Float, Float)] -> IO BetChooser
askForBetChooser thresholds = promptDefaultString "n" "add bet strategy? (y/N)" >>= \case
    n | n `elem` ["n", ""] ->
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
