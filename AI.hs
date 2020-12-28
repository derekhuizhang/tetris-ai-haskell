module AI where

-- Imports
import Data.Matrix
import Types
import Helpers

-- weights type used to calculate the score
data Weights = Weights
  { heightWeight :: Double,
    linesWeight :: Double,
    holesWeight :: Double,
    bumpinessWeight :: Double
  }

-- method to return hardcoded weights for use in scoring function
getWeights :: Weights
getWeights =
  Weights
    { heightWeight = 0.810066,
      linesWeight = 0.760666,
      holesWeight = 0.35663,
      bumpinessWeight = 0.184483
    }

-- score a board using a given set of weights
scoreBoardWithWeights :: Board -> Weights -> Double
scoreBoardWithWeights board weights = linesVal - heightVal - holesVal - bumpinessVal
  where
    heightVal = (heightWeight weights) * (fromIntegral $ aggregateHeight board)
    linesVal = (linesWeight weights) * (fromIntegral $ completeLines board)
    holesVal = (holesWeight weights) * (fromIntegral $ holes board)
    bumpinessVal = (bumpinessWeight weights) * (fromIntegral $ bumpiness board)

-- score a board using the default set of weights
scoreBoard :: Board -> Double
scoreBoard board = scoreBoardWithWeights board getWeights

-- calculate the aggregateHeight of a board
aggregateHeight :: Board -> Int
aggregateHeight board = sum $ getHeights $ toLists $ transpose board

getHeights :: [[Int]] -> [Int]
getHeights lists = map getHeight lists

getHeight :: [Int] -> Int
getHeight [] = 0
getHeight (x : xs)
  | x == 1 = length xs + 1
  | otherwise = getHeight xs

-- count the completeLines in a board where the rows haven't been cleared yet
completeLines :: Board -> Int
completeLines board = length $ getFullRowIndexes 0 [] (toLists board)

-- count the number of enclosed holes in a board
holes :: Board -> Int
holes board = foldl (\count arr -> count + getHolesInArr 0 False arr) 0 transposedBoardList
  where
    transposedBoardList = toLists $ transpose board

getHolesInArr :: Int -> Bool -> [Int] -> Int
getHolesInArr count _ [] = count
getHolesInArr count started (x : xs)
  | (x == 1) && not started = getHolesInArr count True xs
  | (x == 0) && started = getHolesInArr (count + 1) True xs
  | otherwise = getHolesInArr count started xs

-- calculuate the bumpiness of a boards columns
bumpiness :: Board -> Int
bumpiness board = sum $ getHeightDiffs $ getHeights $ toLists $ transpose board

getHeightDiffs :: [Int] -> [Int]
getHeightDiffs diffs = map abs $ zipWith (-) diffs (drop 1 diffs)
