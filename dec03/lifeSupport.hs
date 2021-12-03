{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Bits ()
import Data.List (transpose)
import GHC.IO.Handle (isEOF)

-- gets the most common bit in a bitstring
mostCommonBit :: String -> Char
mostCommonBit bits
  | even total && ones >= (total `div` 2) = '1' -- in even case, >= suffices
  | odd total && ones > (total `div` 2) = '1' -- need to handle odd case, since ['0', '0', '1'] has 1 >= 1 but 1 is not majority
  | otherwise = '0'
  where
    ones = length (filter (== '1') bits)
    total = length bits

-- negation for chars
bnot :: Char -> Char
bnot '0' = '1'
bnot '1' = '0'

-- converts a binary string in big-endian order to an unsigned integer (Word)
binaryStringToWord :: String -> Word
binaryStringToWord bstring = raise (reverse bstring) 0
  where
    raise (b : bs) pow
      | b == '0' = raise bs (pow + 1)
      | b == '1' = 2 ^ pow + raise bs (pow + 1)
      | otherwise = undefined
    raise [] pow = 0

-- reads in the binary strings, line by line
readDiagnostics :: IO [String]
readDiagnostics = do
  done <- isEOF
  if done
    then return []
    else do
      bstring <- getLine
      rest <- readDiagnostics
      return (bstring : rest)

filterOxygen :: Int -> [String] -> String
filterOxygen i nums
  | length matching > 1 = filterOxygen (i + 1) matching -- filter with (i+1)th bit
  | otherwise = head matching -- return the only remaining value
  where
    majorityBit = mostCommonBit (transpose nums !! i) -- computes most common value for the ith bit
    matching = filter ((== majorityBit) . (!! i)) nums -- filters to nums whose ith bit equals b

filterCarbon :: Int -> [String] -> String
filterCarbon i nums
  | length matching > 1 = filterCarbon (i + 1) matching -- filter with (i+1)th bit
  | otherwise = head matching -- return the only remaining value
  where
    majorityBit = (bnot . mostCommonBit) (transpose nums !! i) -- computes least common value for the ith bit
    matching = filter ((== majorityBit) . (!! i)) nums -- filters to nums whose ith bit equals b

main :: IO ()
main = do
  bitstrings <- readDiagnostics
  let oxygenRating = filterOxygen 0 bitstrings -- start at the 0th bit
  let carbonRating = filterCarbon 0 bitstrings -- start at the 0th bit
  -- convert to numbers and multiply
  print (binaryStringToWord oxygenRating * binaryStringToWord carbonRating)
