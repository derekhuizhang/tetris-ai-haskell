-- Imports
import AI
import Control.Applicative
import Control.Monad.Par
import Data.List
import Helpers
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)
import Types

-- MAIN METHOD
-- main method version for reading space-separated integers from 0-6, representing a predefined tetromino order
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      -- read in file
      -- convert to lines
      -- do this function below for all lines in file
      contents <- readFile filename
      let strings = words contents
          li = map read strings :: [Int]
          board = createBoard
          pieceList = map randomPiece li -- list of random pieces
          tupleList = take (length pieceList - 4 + 1) (map (take 4) (tails pieceList))
      gameloop tupleList (Just (BoardMove board Nothing 0))
      return ()

    _ -> do
      pn <- getProgName -- Usage message
      hPutStrLn stderr $ "Usage: " ++ pn ++ " <file name>"
      exitFailure

-- -- ALTERNATE MAIN METHOD
-- -- gets the N input from the user and places N random pieces in their best position starting from an empty board
-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [n] -> do
--       li <- generateNList (read n :: Int)
--       let board = createBoard
--           pieceList = map randomPiece li -- list of random pieces
--           searchDepth = 4
--           -- create a search depth length list of next pieces, currently using a search depth of 4
--           tupleList = take (length pieceList - searchDepth + 1) (map (take searchDepth) (tails pieceList)) 
--       gameloop tupleList (Just (BoardMove board Nothing 0))
--       return ()

--     _ -> do
--       pn <- getProgName -- Usage message
--       hPutStrLn stderr $ "Usage: " ++ pn ++ " <n tetrominos>"
--       exitFailure

-- interactive method for passing in an N and running the game loop for a randomPiece list sequentially
placeN :: Int -> IO ()
placeN n = do li <- generateNList n
              let board = createBoard
                  pieceList = map randomPiece li -- list of random pieces
                  searchDepth = 4
                  -- create a search depth length list of next pieces, currently using a search depth of 4
                  tupleList = take (length pieceList - searchDepth + 1) (map (take searchDepth) (tails pieceList)) 
              gameloop tupleList (Just (BoardMove board Nothing 0))
              return ()

-- recursive helper method for main to place all pieces in a Tetromino list
gameloop :: [[Tetromino]] -> Maybe BoardMove -> IO ()
gameloop _ Nothing = putStrLn "Failed to complete board!"
gameloop [] _ = putStrLn "Finished successfully!"
gameloop (current : rest) (Just (BoardMove board piece score)) = do
  putStrLn $ show (BoardMove board piece score)
  boardMove <- getBestMove current board
  gameloop rest boardMove

-- Given a list of next N Tetrominos and a board, return the best possilbe placement for the first Tetromino in the list
getBestMove :: [Tetromino] -> Board -> IO (Maybe BoardMove)
getBestMove [] _ = return Nothing
getBestMove (Tetromino pieces : xs) board = do
  let li = runPar $ parMap (\(piece, c) -> startNextTick xs (BoardState board piece (5, c))) (liftA2 (,) pieces [1 .. 10])
      ans = getBestBoard li
  return ans

-- return a list of all possible placements of the first Tetromino searching over the search space for the rest of the Tetrominoes to calculate the best placement 
getPossibleBoards :: [Tetromino] -> Board -> [Maybe BoardMove]
getPossibleBoards [] _ = [Nothing]
getPossibleBoards ((Tetromino pieces) : xs) board = map (\(piece, c) -> nextTick xs (BoardState board piece (5, c)) GoDown) (liftA2 (,) pieces [1 .. 10])

-- Action Enum type to represent which action to do in nextTick
data Action = GoDown | Place

-- search the search path for the best board at max depth for a given piece and location representing the column to be placed in
startNextTick :: [Tetromino] -> BoardState -> Maybe BoardMove
startNextTick tetrominos bs = nextTick tetrominos bs GoDown

nextTick :: [Tetromino] -> BoardState -> Action -> Maybe BoardMove
nextTick tetrominos (BoardState board piece loc) GoDown
  | doesNotOverlap (BoardState board piece loc) =
    getBestBoard [nextTick tetrominos (BoardState board piece (fst loc + 1, snd loc)) GoDown, nextTick tetrominos (BoardState board piece (fst loc + 1, snd loc)) Place]
  | otherwise = Nothing
nextTick tetrominos (BoardState board piece loc) Place
  | not $ isValidPlacement (BoardState board piece loc) = Nothing
  | otherwise = do
    newBoard <- putPiece (BoardState board piece loc)
    let newBoardCleared = clearRows newBoard
        bestBoard [] = Nothing
        bestBoard (_ : xs) = getBestBoard $ getPossibleBoards xs newBoardCleared
    return $ BoardMove newBoardCleared (Just piece) (getScore (bestBoard tetrominos) newBoard)
  where
    getScore (Just (BoardMove _ _ score)) _ = score
    getScore Nothing currentBoard = scoreBoard currentBoard

-- given a list of scored boards in BoardMoves, return the board with the highest max score
getBestBoard :: [Maybe BoardMove] -> Maybe BoardMove
getBestBoard boardMoves = getBestBoardHelper Nothing boardMoves

getBestBoardHelper :: Maybe BoardMove -> [Maybe BoardMove] -> Maybe BoardMove
getBestBoardHelper currentBest [] = currentBest
getBestBoardHelper Nothing (x : xs) = getBestBoardHelper x xs
getBestBoardHelper (Just (BoardMove board piece score)) ((Just (BoardMove newBoard newPiece newScore)) : xs)
  | newScore > score = getBestBoardHelper (Just (BoardMove newBoard newPiece newScore)) xs
  | otherwise = getBestBoardHelper (Just (BoardMove board piece score)) xs
getBestBoardHelper currentBest (_ : xs) = getBestBoardHelper currentBest xs
