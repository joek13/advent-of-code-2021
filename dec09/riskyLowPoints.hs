import Data.Char (digitToInt)
import Data.Functor ((<&>))
import GHC.IO.Handle (isEOF)

-- reads a single line of digits and returns the interpreted
-- list of single-digit Ints
readLine :: IO [Int]
readLine = getLine <&> map digitToInt

-- reads each line of input until eof,
-- returning a matrix of int digits
readInput :: IO [[Int]]
readInput = do
  done <- isEOF
  if done
    then return []
    else do
      row <- readLine
      rest <- readInput
      return (row : rest)

-- returns four cells adjacent to a cell.
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) =
  [ (i -1, j),
    (i + 1, j),
    (i, j -1),
    (i, j + 1)
  ]

-- bounds check for a coordinate (i,j)
inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds height width (i, j) = 0 <= i && i < height && 0 <= j && j < width

-- takes in Int matrix, returns list of
-- cell values at "low points": cells which
-- are smaller than all their neighbors
findLowPoints :: [[Int]] -> [Int]
-- get all coordinates (i,j), flatten them so we can process them serially
-- then filter down to those which are low points
findLowPoints mat = map access (filter isLowPoint (concat allCoords))
  where
    -- width of a matrix: length of its first row
    width = (length . head) mat
    -- height of a matrix: length of the list of rows
    height = length mat
    -- construct (i,j) for each cell in the matrix
    allCoords = [[(i, j) | j <- [0 .. width -1]] | i <- [0 .. height -1]]
    -- checks whether a given cell is a low point
    isLowPoint (i, j) =
      let val = mat !! i !! j -- gets the value at (i,j)
      -- checks if it is lower than all of its immediate (in bounds) neighbors
       in all (\(r, c) -> val < (mat !! r !! c)) (filter (inBounds height width) (neighbors (i, j)))
    -- just grabs the value at (i,j)
    access (i, j) = mat !! i !! j

main = do
  input <- readInput
  let lowPoints = findLowPoints input
  print (sum (map succ lowPoints)) -- risk level of a low point = 1 + its height = succ(height)