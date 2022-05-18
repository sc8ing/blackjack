{-# LANGUAGE LambdaCase #-}

module CsvLoading
  ( loadChooseMoveFromCsv,
  )
where

import Control.Monad.Cont
import GameCore
import Text.CSV (CSV, parseCSVFromFile)
import Data.Char (isNumber)
import Data.List.Split (splitOn)

loadChooseMoveFromCsv :: (MonadFail m, MonadIO m) => FilePath -> FilePath -> FilePath -> FilePath -> m MoveChooser
loadChooseMoveFromCsv hard soft split surrender = do
    [ surrenderHandler,
      splitHandler,
      softHandler,
      hardHandler ] <- forM [surrender, split, soft, hard] $ \filePath ->
        liftIO (parseCSVFromFile filePath) >>= \case
            Left parseError -> error $ show parseError
            Right csv -> pure $ \state dealerUp hand ->
                let dealerScore = (handScoreInt . cardSum) [dealerUp]
                    handScore   = (handScoreInt . cardSum) hand
                    rawMove     = getRowColumn csv handScore dealerScore
                    count = trueCount (_shoeDecks . _rules $ state) (_cardsPlayed state)
                in
                case parseRawCsvMove rawMove of
                  Left unconditionalMove ->
                      unconditionalMove
                  Right (aboveMove, threshCount, belowMove) ->
                      if count >= threshCount then aboveMove else belowMove
    pure $ \state dealerUp hand ->
        let runHandler = \handler -> handler state dealerUp hand in
        case cardSum hand of
            Soft handScore ->
                if runHandler splitHandler == Split then Split
                else runHandler softHandler
            Hard handScore ->
                if runHandler surrenderHandler == Surrender then Surrender
                else if runHandler splitHandler == Split then Split
                else runHandler hardHandler

-- | Either a move to make unconditionally, or a tuple (aboveMove, threshCount, belowMove) where
-- | `aboveMove` should be made if the count is at or above `threshCount` and `belowMove` otherwise
parseRawCsvMove :: String -> Either Move (Move, Float, Move)
parseRawCsvMove rawMove =
    case splitOn "/" rawMove of
        [move] -> Left (read move :: Move)
        [moveCond, alt] ->
            let move = read $ takeWhile (not . isNumber) moveCond
                altMove = read alt :: Move
                countThresh = read $ dropWhile (not . isNumber) moveCond
            in
            Right (move, countThresh, altMove)
        _ -> error ""

getRowColumn :: CSV -> Int -> Int -> String
getRowColumn csv row column =
    csv !! row !! column
