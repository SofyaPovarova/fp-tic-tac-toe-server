{-# LANGUAGE RecordWildCards #-}

module Game where

import Models
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Lens.Micro ((.~), (^.))

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

checkWinAfterMove :: Int -> Int -> Field -> Maybe Cell
checkWinAfterMove x y field = do
  let cell = (field^.fieldCells) ! (x, y)
  let xs = [x-2..x+2]
  let ys = [y-2..y+2]
  let row = zip (repeat y) xs  
  let column = zip ys (repeat x)
  let leftDiagonal = zip xs ys
  let rightDiagonal = zip xs (reverse ys)
  let win = any check [row, column, leftDiagonal, rightDiagonal]
  if win then Just cell else Nothing
    where
      check :: [(Int, Int)] -> Bool
      check indices = any (\p -> checkWinCondition (slice p indices) field) $ zip [0..2] [2..4]
 
checkWinCondition :: [(Int, Int)] -> Field -> Bool
checkWinCondition indices Field{_fieldCells = cells} =
  case traverse (`Map.lookup` cells) indices of
    Just cs -> all (== head cs) cs
    Nothing -> False

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