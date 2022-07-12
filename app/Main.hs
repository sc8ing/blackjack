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
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "loading initial strategy"
  initStrat <- askForStrat
  stratRef  <- newIORef initStrat
  repl stratRef

repl :: IORef Strategy -> IO ()
repl stratRef = (words <$> getLine) >>= \case
  ["play", n] -> case (readMaybe n :: Maybe Int) of
    Nothing ->
      putStrLn "play [number of shoes]" >> repl stratRef
    Just n -> do
      strat <- readIORef stratRef
      result <- playShoes strat n
      print result
      repl stratRef
  ["load", "move"] -> do
    move <- askForMoveChooser
    modifyIORef stratRef (\s -> s { _chooseMove = move })
  ["load", "bet"] -> do
    bet <- askForBetChooser
    modifyIORef stratRef (\s -> s { _chooseBet = bet })
  ["load", "insurance"] -> do
    ins <- askForInsuranceChooser
    modifyIORef stratRef (\s -> s { _chooseInsurance = ins })
  ["load"] ->
    askForStrat >>= writeIORef stratRef
  h : _ | h `elem` ["q", "quit"]  ->
    putStrLn "bye, keep gambling!"
  _ ->
    putStrLn "not an understood command" >> repl stratRef

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
