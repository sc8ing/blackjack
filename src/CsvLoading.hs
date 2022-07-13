{-# LANGUAGE LambdaCase #-}

module CsvLoading
  ( loadChooseMoveFromCsv,
  )
where

import Control.Monad.Cont
import Data.Char (isNumber)
import Data.List.Split (splitOn)
import Debug.Trace (trace, traceShowId)
import GameCore
import Text.CSV (CSV, parseCSVFromFile)
import Text.Printf (printf)

loadChooseMoveFromCsv :: (MonadFail m, MonadIO m) =>
    FilePath ->
    FilePath ->
    FilePath ->
    FilePath ->
    m MoveChooser
loadChooseMoveFromCsv hard soft split surrender = do
    [ rawSurrenderHandler,
      softHandler,
      hardHandler ] <- forM [surrender, soft, hard] $ \filePath ->
          liftIO (parseCSVFromFile filePath) >>= \case
              Left parseError -> error $ show parseError
              Right csv -> pure $ \state dealerUp hand ->
                  let dealerScore = if dealerUp == Ace then 11
                                    else (handScoreInt . cardSum) [dealerUp]
                      handScore   = (handScoreInt . cardSum) hand
                      rawMove     = getRowColumn csv (22 - handScore) (dealerScore - 1)
                      count       = trueCountFromGameState state
                  in
                  case parseRawCsvMove rawMove of
                      Left unconditionalMove ->
                          unconditionalMove
                      Right (aboveMove, threshCount, belowMove) ->
                          if count >= threshCount then aboveMove else belowMove
    splitHandler <- liftIO (parseCSVFromFile split) >>= \case
        Left parseError -> error $ show parseError
        Right csv -> pure $ \state dealerUp hand -> case hand of
                                                        [c1, c2] | c1 == c2 ->
                                                            let dealerScore = if dealerUp == Ace then 11
                                                                              else (handScoreInt . cardSum) [dealerUp]
                                                                cardNum = (handScoreInt . cardSum) [c1]
                                                                count = trueCountFromGameState state
                                                                rawMove = getRowColumn csv (12 - cardNum) (dealerScore - 1)
                                                            in
                                                            Just $ case parseRawCsvMove rawMove of
                                                                Left unconditionalMove ->
                                                                    unconditionalMove
                                                                Right (aboveMove, threshCount, belowMove) ->
                                                                    if count >= threshCount then aboveMove else belowMove
                                                        _ -> Nothing
    let wrapHandler validHandlerMove handler = \s up hand -> mfilter (== validHandlerMove) $ Just (handler s up hand)
        [surrenderHandler] = [ wrapHandler Surrender rawSurrenderHandler ]
    pure $ \state dealerUp hand ->
        let count = trueCountFromGameState state in
        trace (show hand <> " vs " <> show dealerUp <> " (true " <> printf "%.1g" count <> "): ") $ traceShowId $
        let runHandler = \handler -> handler state dealerUp hand
            maybeMove = dropWhile (== Nothing) $ runHandler <$> [trace "running surrender" surrenderHandler, trace "running split" splitHandler,
                                    \s up hand -> Just $ (case cardSum hand of
                                        Soft _ -> trace "running soft" softHandler
                                        Hard _ -> trace "running hard" hardHandler) s up hand
                                  ]
        in
        case maybeMove of
            Just move : _ -> move
            _ -> error "oh no"

-- | Either a move to make unconditionally, or a tuple (aboveMove, threshCount, belowMove) where
-- | `aboveMove` should be made if the count is at or above `threshCount` and `belowMove` otherwise
parseRawCsvMove :: String -> Either Move (Move, Float, Move)
parseRawCsvMove rawMove =
    case splitOn "/" rawMove of
        [move] -> Left (read move :: Move)
        [moveCond, alt] ->
            let isPartOfCount c = isNumber c || c == '-' -- minus sign
                move = read $ takeWhile (not . isPartOfCount) moveCond
                altMove = read alt :: Move
                countThresh = read $ dropWhile (not . isPartOfCount) moveCond
            in
            trace ("parsed move from " <> rawMove) $ traceShowId $ Right (move, countThresh, altMove)
        _ -> error $ "invalid move from CSV: " <> rawMove

getRowColumn :: CSV -> Int -> Int -> String
getRowColumn csv row column =
    csv !! row !! column
