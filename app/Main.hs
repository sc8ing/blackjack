{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import GameCore
import GameExecution (playShoes)

main :: IO ()
main = playShoes 50 >>= print
