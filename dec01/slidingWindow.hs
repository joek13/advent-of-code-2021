import Data.List
import System.IO (isEOF)

-- reads input into a list of integer heights
readHeights :: IO [Int]
readHeights = do
  -- check if we have reached end of stream
  done <- isEOF
  if done
    then -- if so, just return empty list
      return []
    else do
      -- read in current line as Int
      height <- readLn
      -- read in all future heights
      next <- readHeights
      return (height : next)

slidingWindows ::
  -- | length of the sliding window
  Int ->
  -- | list to generate windows from
  [a] ->
  [[a]]
-- generate all of the tails of a list
-- and take the first n elements of each tail
slidingWindows n as = filter complete (map (take n) (tails as))
  where
    complete = (>= n) . length -- filter resulting windows to those that have the correct length
    -- without the filter, slidingWindows 3 [1,2,3,4] returns [[1,2,3], [2,3,4], [3,4], [4], []]

-- given a list, returns a list of pairs where the ith pair represents (a_i, a_i+1)
adjPairs :: [a] -> [(a, a)]
adjPairs as = zip as (drop 1 as)

main = do
  -- read inputs
  heights <- readHeights
  -- build 3-length sliding windows
  let windows = slidingWindows 3 heights
  -- organize into pairs of (window_i, window_i+1)
  -- select only pairs for which window_i+1 has sum greater than window_i
  let increases = filter isIncrease (adjPairs windows)
  -- count number of such pairs
  print (length increases)
  where
    isIncrease (windowA, windowB) = sum windowB > sum windowA
