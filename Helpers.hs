module Helpers where

-- Imports
import Data.Matrix
import System.Random
import Types

-- generates n random numbers in a list, helper for creating random Tetromino list
generateNList :: Int -> IO [Int]
generateNList n = sequence $ replicate n $ randomRIO (0, 6 :: Int)

-- maps an integer to a Tetromino, helper for creating random Tetromino list
randomPiece :: Int -> Tetromino
randomPiece r = case r of
  0 -> tetrominoI
  1 -> tetrominoO
  2 -> tetrominoS
  3 -> tetrominoZ
  4 -> tetrominoT
  5 -> tetrominoJ
  6 -> tetrominoL
  _ -> error "invalid piece"

-- BOARD METHODS

-- creates empty board, starting point for placeN call
createBoard :: Matrix Int
createBoard = zero 25 10

-- takes top 5 rows off of matrix (buffer rows for piece placement) and prints the board
printBoard :: Board -> IO ()
printBoard board = do
  let slice = submatrix 6 25 1 10 board
  print slice
  return ()

-- clears rows of all 1s from a board
clearRows :: Board -> Board
clearRows board = listsAsMatrix
  where
    matrixAsLists = toLists board
    clearFromRows = foldr removeIndex matrixAsLists (getFullRowIndexes 0 [] matrixAsLists)
    removeIndex index list = [replicate 10 0] ++ take index list ++ tail (drop index list)
    listsAsMatrix = fromLists clearFromRows

-- helper for clearing rows, get indices of all full 1 rows
getFullRowIndexes :: Int -> [Int] -> [[Int]] -> [Int]
getFullRowIndexes _ currList [] = currList
getFullRowIndexes currIndex currList (x : xs) = getFullRowIndexes (currIndex + 1) (appendedCurrList x currIndex currList) xs
  where
    appendedCurrList vals index list
      | all (== 1) vals = index : list
      | otherwise = list

-- BOARDSTATE METHODS

-- converts a BoardState into a Board by placing its piece at its location
putPiece :: BoardState -> Maybe Board
putPiece (BoardState board piece loc) = foldr fillLoc (Just board) piece
  where
    fillLoc _ Nothing = Nothing
    fillLoc pieceLoc (Just myBoard) = safeSet 1 (fst loc - fst pieceLoc, snd loc + snd pieceLoc) myBoard

-- determines if the piece in BoardState could be placed
isValidPlacement :: BoardState -> Bool
isValidPlacement (BoardState board piece loc) = doesNotOverlap (BoardState board piece loc) && any isOnGround piece && any isBelowBuffer piece && fst loc > 5
  where
    isBelowBuffer (r, _) = fst loc - r > 5
    isOnGround (r, c)
      | safeGet (fst loc - r + 1) (snd loc + c) board == Just 1 = True
      | (fst loc - r + 1 == nrows board + 1) && (r == 0) = True
      | otherwise = False

-- helper function to isValidPlacement to check if a piece overlaps with already placed pieces
doesNotOverlap :: BoardState -> Bool
doesNotOverlap (BoardState board piece loc) = all (isValidLoc . safeGetVal) piece
  where
    safeGetVal (r, c) = safeGet (fst loc - r) (snd loc + c) board
    isValidLoc (Just 0) = True
    isValidLoc _ = False
