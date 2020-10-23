{-# LANGUAGE RecordWildCards #-}

module Game
  ( GameError(..)
  , ComputerMoveResult(..)
  , emptyField
  , checkWinAfterMove
  , move
  , computerMakeMove
  ) where

import qualified Data.Map.Strict as Map

import Data.Map.Strict ((!))
import Data.List ((\\))
import Lens.Micro ((.~), (^.))
import System.Random (randomRIO)

import Models

data ComputerMoveResult = ComputerMoveResult (Int, Int) Field

data GameError = CellNotEmptyError | OutOfBoundsError

emptyField :: Int -> Cell -> Either String Field
emptyField size playerRole
  | size < 3 = Left "Size should be at least 3"
  | playerRole == X = Right $ Field { _fieldCells = Map.empty
                                    , _fieldSize = size
                                    }
  | otherwise = Right $ Field { _fieldCells = Map.fromList [((center, center), X)]
                              , _fieldSize = size
                              }
      where
        center = size `div` 2

checkWinAfterMove :: (Int, Int) -> Field -> Int -> Maybe GameResult
checkWinAfterMove (x, y) field lineLength
  | lineLength <= 1 = Nothing
  | otherwise = do
      let cell = (field^.fieldCells) ! (x, y)
      let lookupRange = lineLength - 1
      let xs = [x - lookupRange .. x + lookupRange]
      let ys = [y - lookupRange .. y + lookupRange]
      let row = zip xs (repeat y)
      let column = zip (repeat x) ys
      let leftDiagonal = zip xs ys
      let rightDiagonal = zip xs (reverse ys)
      let win = any (check lookupRange) [row, column, leftDiagonal, rightDiagonal]
      if win then Just (toGameResult cell) else Nothing
        where
          check :: Int -> [(Int, Int)] -> Bool
          check lookupRange indices =
            any (\range -> checkWinCondition (slice range indices) field) $
              zip [0..lookupRange] [lookupRange..lookupRange * 2]

checkWinCondition :: [(Int, Int)] -> Field -> Bool
checkWinCondition indices Field{_fieldCells = cells} =
  case traverse (`Map.lookup` cells) indices of
    Just cs -> all (== head cs) cs
    Nothing -> False

computerMakeMove :: Field -> Cell -> IO (Maybe ComputerMoveResult)
computerMakeMove field cell = do
  let range = [0 .. field^.fieldSize - 1]
  let indices = [(x, y) | x <- range, y <- range]
  let cells = Map.keys $ field^.fieldCells
  let freeCells = indices \\ cells
  let freeCellsSize = length freeCells

  case freeCellsSize of
    0 -> return Nothing
    _ -> do
      randomIndex <- randomRIO (0, freeCellsSize - 1)
      let (x, y) = freeCells !! randomIndex
      return $ case move x y cell field of
        Right newField -> Just $ ComputerMoveResult (x, y) newField
        Left _ -> Nothing

move :: Int -> Int -> Cell -> Field -> Either GameError Field
move x y cell field@(Field cells size)
  | notInBounds = Left OutOfBoundsError
  | otherwise =
      case (Map.lookup (x, y) cells) of
        Nothing -> Right $ insertCell field
        _ -> Left CellNotEmptyError
    where
      notInBounds = x >= size || y >= size || x < 0 || y < 0
      insertCell = fieldCells .~ Map.insert (x, y) cell cells

slice :: (Int, Int) -> [a] -> [a]
slice (from, to) xs = take (to - from + 1) (drop from xs)
