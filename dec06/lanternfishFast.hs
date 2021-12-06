{-

idea: instead of representing each fish as a number,
observe that fish which are the same age behave
identically

so we can just maintain a count of fish of each age

the simulation step just involves changing those counts

-}

readInput :: IO [Int]
readInput = do
  line <- getLine
  -- surround with '[' ']' so we can just use read for lists
  return (read ('[' : (line ++ "]")))

-- takes list of fish ages, and returns
-- a list of 9 numbers where the value at index i
-- indicates how many fish there are of age i
makeAgeMap :: [Int] -> [Int]
makeAgeMap fishes = map countFish [0 .. 8]
  where
    countFish age = length (filter (== age) fishes)

-- how many fish there are of age i after a step
stepAge :: [Int] -> Int -> Int
stepAge ageMap age
  | age == 6 = head ageMap + ageMap !! 7 -- all the age 0 fish become age 6, all the age 7 fish become age 6
  | age == 8 = head ageMap -- all the age 0 fish have age 8 kids
  | otherwise = ageMap !! (age + 1) -- each other age just grabs the value above it

step :: [Int] -> [Int]
step ageMap = map (stepAge ageMap) [0 .. 8]

main :: IO ()
main =
  do
    input <- readInput
    let ageMap = makeAgeMap input
    print (sum (iterate step ageMap !! 256))
