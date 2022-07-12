{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Redundant <$>" #-}
module Main where

import GameCore
import GameExecution (playShoes)
import CsvLoading (loadChooseMoveFromCsv)
import Data.IORef
import GHC.IO (catch)
import Text.Read (readMaybe, readEither)

main :: IO ()
main = do
  putStrLn "loading initial strategy"
  initBank  <- prompt "Starting bankroll: "
  initRules <- askForRules
  initStrat <- askForStrat
  initShoe  <- makeShoe (_shoeDecks initRules) (_penetration initRules)
  stateRef  <- newIORef GameState { _cardsUnplayed = initShoe
                                  , _cardsPlayed = []
                                  , _bankroll = initBank
                                  , _rules = initRules
                                  , _playerStrategy = initStrat }
  repl stateRef

repl :: IORef GameState -> IO ()
repl stateRef = (words <$> getLine) >>= \case
  ["play", n] -> case (readMaybe n :: Maybe Int) of
    Nothing ->
      putStrLn "play [number of shoes]" >> repl stateRef
    Just n -> do
      strat <- readIORef stateRef
      result <- playShoes strat n
      print result
      repl stateRef
  ["set", "bankroll", n] ->
    case (readMaybe n :: Maybe Float) of
      Nothing ->
        putStrLn "set bankroll [dollars]" >> repl stateRef
      Just n -> do
        modifyIORef stateRef (\s -> s { _bankroll = n })
  "load" : toLoad ->
    let modifyStrat f =
          readIORef stateRef >>= (\curState ->
            -- guess I should get lenses for this
            modifyIORef stateRef (\s -> s { _playerStrategy = f (_playerStrategy curState) })
          )
    in
    case toLoad of
      ["move"] -> do
        move <- askForMoveChooser
        modifyStrat (\s -> s { _chooseMove = move })
      ["bet"] -> do
        bet <- askForBetChooser
        modifyStrat (\s -> s { _chooseBet = bet })
      ["insurance"] -> do
        ins <- askForInsuranceChooser
        modifyStrat (\s -> s { _chooseInsurance = ins })
      [] -> do
        strat <- askForStrat
        modifyStrat (const strat)
      _ ->
        putStrLn "can load move, bet, or insurance" >> repl stateRef
  h : _ | h `elem` ["q", "quit"]  ->
    putStrLn "bye, keep gambling!"
  _ ->
    putStrLn "not an understood command" >> listCommands >> repl stateRef

listCommands :: IO ()
listCommands =
  putStrLn "commands are play [shoes], set bankroll [size], load [move/bet/insurance]"

prompt :: Read a => String -> IO a
prompt toSay = do
  putStrLn toSay
  input <- getLine
  case readEither input of
    Right a ->
      pure a
    Left err ->
      putStrLn err >> prompt toSay

askForRules :: IO Rules
askForRules =
  Rules <$> prompt "How many decks?"
        <*> prompt "Percent penetration: "
        <*> prompt "Min bet: "

askForStrat :: IO Strategy
askForStrat =
  Strategy <$> askForMoveChooser <*> askForBetChooser <*> askForInsuranceChooser

askForMoveChooser :: IO MoveChooser
askForMoveChooser =
  putStrLn "give four arguments for hard, soft, split, & surrender charts" >>
  (words <$> getLine) >>= \case
  [hard, soft, split, surrender] ->
    loadChooseMoveFromCsv hard soft split surrender
  _ ->
    askForMoveChooser

askForBetChooser :: IO BetChooser
askForBetChooser = pure $ const 5

askForInsuranceChooser :: IO InsuranceChooser
askForInsuranceChooser = pure $ const . const True
