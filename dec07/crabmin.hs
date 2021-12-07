import Data.List

{-

motivation:
    while the mean minimizes the sum of squared error,
    the median minimizes the sum of |error|

    so if we find the median crab position, we know that
    is the optimal place to align our crabs

-}

-- read input list of crab horiz positions
readInput :: IO [Int]
readInput = do
  line <- getLine
  -- surround with '[' ']' so we can just use read for lists
  return (read ('[' : (line ++ "]")))

-- finds the median in a sorted list of Ints.
median :: [Int] -> Int
median as
  | odd (length as) = as !! mid
  | otherwise = (as !! mid + as !! (mid - 1)) `div` 2
  where
    mid = length as `div` 2

-- comptues sum | x_i - m |
computeResiduals :: Int -> [Int] -> Int
computeResiduals val = sum . map (abs . (val -))

main :: IO ()
main = do
  crabPositions <- readInput
  let sortedPositions = sort crabPositions
  let m = median sortedPositions
  print (computeResiduals m sortedPositions)
