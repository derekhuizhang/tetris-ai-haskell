{-# LANGUAGE DeriveGeneric #-}

module Types where

-- Imports
import Data.Matrix
import Control.DeepSeq
import GHC.Generics (Generic)

-- (row, column) representation for the matrix board representation
type Location = (Int, Int)

-- a piece represents a unique rotation of a Tetromino
type Piece = [Location]

-- a representation of the game Board
type Board = Matrix Int

-- Tetromino is a list of all possible rotations of a Tetromino
newtype Tetromino = Tetromino [Piece]

-- BoardState is an intermediate state for the search representing a board, a piece, and its current location
data BoardState = BoardState Board Piece Location

-- Used to represent the score of a Board and the most recently placed piece
data BoardMove = BoardMove Board (Maybe Piece) Double deriving Generic

instance NFData BoardMove

-- show method for BoardMove
instance Show BoardMove where
  show (BoardMove board piece score) = "\nScore: " ++ show score ++ "\n Tetromino: \n" ++ tetrominoBoard ++ "\nBoard:\n" ++ show slice
    where
      tetrominoBoard = tetrominoDisplay blankBoard piece
      blankBoard = zero 4 4
      slice = submatrix 6 25 1 10 board

-- print method for displaying a tetromino onto a board
tetrominoDisplay :: Board -> Maybe Piece -> String
tetrominoDisplay _ Nothing = "No tetromino selected"
tetrominoDisplay board (Just []) = show board
tetrominoDisplay board (Just (x : xs)) = tetrominoDisplay (setElem 1 ((nrows board) - (fst x), 1 + snd x) board) (Just xs)

-- all tetromino instances
tetrominoI :: Tetromino
tetrominoI = Tetromino [[(0, 0), (1, 0), (2, 0), (3, 0)], [(0, 0), (0, 1), (0, 2), (0, 3)]]

tetrominoO :: Tetromino
tetrominoO = Tetromino [[(0, 0), (0, 1), (1, 0), (1, 1)]]

tetrominoZ :: Tetromino
tetrominoZ = Tetromino [[(1, 0), (1, 1), (0, 1), (0, 2)], [(0, 0), (1, 0), (1, 1), (2, 1)]]

tetrominoS :: Tetromino
tetrominoS = Tetromino [[(0, 0), (0, 1), (1, 1), (1, 2)], [(0, 1), (1, 1), (1, 0), (2, 0)]]

tetrominoT :: Tetromino
tetrominoT =
  Tetromino
    [ [(0, 0), (0, 1), (0, 2), (1, 1)],
      [(0, 0), (1, 0), (2, 0), (1, 1)],
      [(1, 0), (1, 1), (1, 2), (0, 1)],
      [(1, 0), (0, 1), (1, 1), (2, 1)]
    ]

tetrominoJ :: Tetromino
tetrominoJ =
  Tetromino
    [ [(0, 0), (0, 1), (1, 1), (2, 1)],
      [(0, 0), (1, 0), (0, 1), (0, 2)],
      [(0, 0), (1, 0), (2, 0), (2, 1)],
      [(1, 0), (1, 1), (1, 2), (0, 2)]
    ]

tetrominoL :: Tetromino
tetrominoL =
  Tetromino
    [ [(0, 0), (1, 0), (2, 0), (0, 1)],
      [(0, 0), (1, 0), (1, 1), (1, 2)],
      [(2, 0), (2, 1), (1, 1), (0, 1)],
      [(0, 0), (0, 1), (0, 2), (1, 2)]
    ]