{-# LANGUAGE LambdaCase #-}

module CsvLoading
  ( loadChooseMoveFromCsv,
  )
where

import Control.Monad.Cont
import Data.Char (isNumber)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map ((!?))
import Debug.Trace (trace, traceShowId)
import GameCore
import qualified Data.Map as Map
import Text.CSV (CSV, parseCSVFromFile, Field)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Either a move to make no matter what or a move to make when >= some count, otherwise some other move
type CondMove = Either Move (Move, Float, Move)

loadChooseMoveFromCsv :: (MonadFail m, MonadIO m) =>
    FilePath ->
    FilePath ->
    FilePath ->
    FilePath ->
    m MoveChooser
loadChooseMoveFromCsv hard soft split surrender = do
    [ rawSurrenderHandler,
      softHandler,
      hardHandler,
      rawSplitHandler ] <- forM [surrender, soft, hard, split] $ \filePath -> liftIO (parseCSVFromFile filePath) >>= \case
        Left parseError ->
            error $ "failed to parse csv from " <> show filePath <> ": " <> show parseError
        Right csv ->
            -- Drop the header row cause this pays attention to the order of columns not their labels
            -- Remove the empty row that's sometimes added to the end and confuses this poor simple parser
            let processedCsv = filter (/= [""]) $ drop 1 csv in
            pure $ \state dealerUp hand -> case loadMove processedCsv hand dealerUp of
                Left errMsg ->
                    error $ "couldn't load move from " <> show csv <> ": " <> errMsg
                Right (Left unconditionalMove) ->
                    unconditionalMove
                Right (Right (aboveMove, threshCount, belowMove)) ->
                    if trueCountFromGameState state >= threshCount then aboveMove else belowMove
    -- Anything moves in the surrender other than surrender should be turned to Nothings
    -- I guess ideally this should be refactored to allow for blanks in the CSVs instead of bs filler moves.
    -- Or preprocess them.
    let onlyAllow validHandlerMove handler = \s up hand -> mfilter (== validHandlerMove) $ Just (handler s up hand)
        [surrenderHandler, splitHandler] = [onlyAllow Surrender rawSurrenderHandler, onlyAllow Split rawSplitHandler]
    pure $ \state dealerUp hand ->
        let count = trueCountFromGameState state in
        trace (show hand <> " (" <> show (cardSum hand) <> ") vs " <> show dealerUp <> " (true " <> printf "%.1g" count <> "): ") $ traceShowId $
        let runHandler = \handler -> handler state dealerUp hand
            maybeMove = dropWhile (== Nothing) $ runHandler <$> [
                              trace "running surrender" surrenderHandler
                            , case hand of
                                  [c1, c2] | c1 == c2 -> trace "running split" splitHandler
                                  _ -> const . const . const Nothing
                            , \s up hand -> Just $ (case cardSum hand of
                                  Soft _ -> trace "running soft" softHandler
                                  Hard _ -> trace "running hard" hardHandler) s up hand
                        ]
        in
        case maybeMove of
            Just move : _ ->
                move
            _ ->
                error $ "was unable to determine a move to make for " <> show hand <> " vs " <> show dealerUp

mapCells :: CSV -> (Field -> Field) -> CSV
mapCells csv f = fmap (fmap f) csv

loadMove :: CSV -> Hand -> Card -> Either String CondMove
loadMove csv hand dealerUp =
     let err msg = Left $ msg <> " - context: " <> show hand <> " vs " <> show dealerUp <> ", CSV " <> show csv
         makeRowToTuple (hand : moves) = do
             parsedMoves <- traverse parseRawCsvMove moves
             key <- case splitOn "-" hand of
                      [c1, c2] ->
                          case traverse (readMaybe :: String -> Maybe Int) [c1, c2] of
                            Nothing -> err $ "couldn't parse " <> show [c1, c2] <> " as int"
                            Just ints -> Right $ Left ints
                      [n] ->
                          case readMaybe n :: Maybe Int of
                            Nothing -> err $ "couldn't parse " <> show n <> " as an int for hand " <> hand <> ", moves " <> show moves
                            Just int -> Right $ Right int
                      r ->
                          err $ "invalid player hand key: " <> show r
             pure (key, parsedMoves)
         makeRowToTuple row = err $ "invalid csv row: " <> show row
     in do
         parsedRows <- traverse makeRowToTuple csv
         let handToRow = Map.fromList parsedRows
             dealerScore = handIntValue [dealerUp]
             specificHandKey = sort (handIntValue . (: []) <$> hand)
             numericKey = handIntValue hand
             maybeMove = do
                 row <- case handToRow !? Left specificHandKey of
                          Nothing -> handToRow !? Right numericKey
                          justrow -> justrow
                 row !?? (dealerScore - 2)
         case maybeMove of
           Nothing -> Left $ "couldn't figure out move for " <> show hand <> " vs " <> show dealerUp
           Just move -> pure move

-- Get the nth item of a list. Like !! but Maybe
(!??) :: [a] -> Int -> Maybe a
(a : _) !?? 0 = Just a
(a : as) !?? n | n > 0 = as !?? (n - 1)
_ !?? _ = Nothing

parseRawCsvMove :: String -> Either String CondMove
parseRawCsvMove rawMove =
    case splitOn "/" rawMove of
        [move] -> Right $ Left (read move :: Move)
        [moveCond, alt] ->
            let isPartOfCount c = isNumber c || c == '-' -- minus sign
                move = read $ takeWhile (not . isPartOfCount) moveCond
                altMove = read alt :: Move
                countThresh = read $ dropWhile (not . isPartOfCount) moveCond
            in
            Right $ Right (move, countThresh, altMove)
        _ -> Left $ "invalid move from CSV: " <> rawMove
