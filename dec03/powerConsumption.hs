{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Bits ()
import Data.List (transpose)
import GHC.IO.Handle (isEOF)

-- gets the most common bit in a bitstring
mostCommonBit :: String -> Char
mostCommonBit bits
  | length (filter (== '1') bits) > (length bits `div` 2) = '1'
  | otherwise = '0'

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

readDiagnostics :: IO [String]
readDiagnostics = do
  done <- isEOF
  if done
    then return []
    else do
      bstring <- getLine
      rest <- readDiagnostics
      return (bstring : rest)

main :: IO ()
main = do
  bitstrings <- readDiagnostics
  let gamma = map mostCommonBit (transpose bitstrings)
  let epsilon = map bnot gamma
  let g' = binaryStringToWord gamma
  let e' = binaryStringToWord epsilon
  print (g' * e')
