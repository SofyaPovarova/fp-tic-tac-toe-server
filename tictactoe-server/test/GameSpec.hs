module GameSpec where

import qualified Data.Map.Strict as Map

import Test.Hspec
import Control.Monad.Reader (liftIO)
import Data.List ((\\))
import Data.Either (fromRight)

import Models
import Game

field :: Field
field = 
  Field
    { _fieldCells = Map.fromList xSquare
    , _fieldSize = 5
    }
  where
    xSquare :: [((Int, Int), Cell)]
    xSquare = [((x, y), X) | x <- [1..3], y <- [1..3]]

spec :: Spec
spec = do
  describe "checkWinAfterMove" $ do
    it "should correctly check win to all directions" $ do
      let cellsToCheck = [(x, y) | x <- [0, 2, 4], y <- [0, 2, 4]] \\ [(2, 2)]
      let checked = traverse (\m@(x, y) -> checkWinAfterMove m (fromRight field $ move x y X field) 4) cellsToCheck
      checked `shouldBe` (Just $ replicate 8 CrossesWin)
    it "should not win with other cell" $ do
      let cellsToCheck = [(x, y) | x <- [0, 2, 4], y <- [0, 2, 4]] \\ [(2, 2)]
      let checked = traverse (\m@(x, y) -> checkWinAfterMove m (fromRight field $ move x y O field) 4) cellsToCheck
      checked `shouldBe` Nothing

  describe "move" $ do
    it "should generate field with new cell if it is free" $ do
      let expected = field
            { _fieldCells = Map.insert (0, 0) X (_fieldCells field)
            }
      move 0 0 X field `shouldBe` Right expected
    it "should produce error if cell not free" $ do
      move 1 1 X field `shouldBe` Left CellNotEmptyError
    it "should produce error if indices out of bounds" $ do
      move 6 0 X field `shouldBe` Left OutOfBoundsError

  describe "computerMakeMove" $ do
    it "should return Nothing if there is no free cells" $ do
      let fullField = Field
            { _fieldCells = Map.fromList [((x, y), X) | x <- [0..2], y <- [0..2]]
            , _fieldSize = 3
            }
      afterMove <- liftIO $ computerMakeMove fullField O
      afterMove `shouldBe` Nothing