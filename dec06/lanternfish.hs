readInput :: IO [Int]
readInput = do
  line <- getLine
  -- surround with '[' ']' so we can just use read for lists
  return (read ('[' : (line ++ "]")))

stepFish :: Int -> [Int]
stepFish days
  | days > 0 = [days - 1] -- just decrement the timer
  | otherwise = [6, 8] -- reset own timer to 6, create new fish with timer of 8 days

step :: [Int] -> [Int]
step = concatMap stepFish -- call stepFish on each fish

main :: IO ()
main = do
  input <- readInput
  -- iterate creates list of repeated application of the simulation step
  -- entry with idx 80 is after 80 days
  print (length (iterate step input !! 80))