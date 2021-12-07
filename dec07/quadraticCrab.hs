import Data.Foldable
import Data.Function

{-

part 2: steps now cost more fuel for each additional step
first step costs 1, second costs 2, etc.

in general if we make n steps, we pay n(n-1) / 2 fuel
     = (n^2 - n)/2

the optimal value m will minimize
    sum ((x_i - m)^2 - |x_i - m|) / 2

-}

-- read input list of crab horiz positions
readInput :: IO [Int]
readInput = do
  line <- getLine
  -- surround with '[' ']' so we can just use read for lists
  return (read ('[' : (line ++ "]")))

-- given a value, and all the crab positions,
-- returns the fuel cost of aligning at that value
fuelCost :: Int -> [Int] -> Int
fuelCost val = sum . map (calcCost val)
  where
    calcCost val to =
      let n = abs (to - val)
       in (n * (n + 1)) `div` 2

-- given a function f : a -> b
-- and a list of candidate as,
-- returns (a, f a) for the a <- as that minimizes f a
argmin :: Ord b => (a -> b) -> [a] -> (a, b)
argmin f as = minimumBy (compare `on` snd) pairs
  where
    pairs = map (\a -> (a, f a)) as

main :: IO ()
main = do
  -- read in the crab positions
  crabPositions <- readInput
  -- minimum crab pos
  let minCrabPosition = minimum crabPositions
  -- max crab pos
  let maxCrabPosition = maximum crabPositions
  -- try every value, select lowest fuel cost
  print (argmin (`fuelCost` crabPositions) [minCrabPosition .. maxCrabPosition])