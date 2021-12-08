import Data.List
import Data.Maybe
import Data.Text (pack, splitOn, unpack)
import GHC.IO.Handle

{-

idea: we can view an assigment of wires to segments
as a permuation of "abcdefg"

the seven-segment display, then, has these indices:

 0000
1    2
1    2
 3333
4    5
4    5
 6666

when the wires are unjumbled, the assignment is just "abcdefg"

if we see the string "bg", we know that it represents a 1
but we don't know whether to pair b <=> 2 and g <=> 5 or
b <=> 5 and g <=> 2

there are only 7! = 5040 possible assignments, so we can just
maintain a list of all viable assignments, and then cut them
down as we encounter numbers.

-}

readInputLine :: IO ([String], [String])
readInputLine = do
  line <- getLine
  let split = splitOn (pack "|") (pack line)
  let notesWords = words (unpack (head split))
  let outputWords = words (unpack (last split))
  return (notesWords, outputWords)

readInputs :: IO [([String], [String])]
readInputs = do
  done <- isEOF
  if done
    then return []
    else do
      this <- readInputLine
      rest <- readInputs
      return (this : rest)

-- list of all segments that make valid digits.
validDigits =
  [ [0, 1, 2, 4, 5, 6], -- zero
    [2, 5], -- one
    [0, 2, 3, 4, 6], -- two
    [0, 2, 3, 5, 6], -- three
    [1, 2, 3, 5], -- four
    [0, 1, 3, 5, 6], -- five
    [0, 1, 3, 4, 5, 6], -- six
    [0, 2, 5], -- seven
    [0, 1, 2, 3, 4, 5, 6], -- eight
    [0, 1, 2, 3, 5, 6] -- nine
  ]

-- accepts a mapping of digits (a permutation of "abcdefg")
-- and a scrambled digit. applies the mapping, yielding a
-- list of activated segments in sorted order
mapDisplay :: String -> String -> [Int]
mapDisplay mapping = sort . map (fromJust . (`elemIndex` mapping))

-- accepts a mapping, and a sample digit
-- returns True <=> it yields a valid digit under this mapping
validMapping :: String -> String -> Bool
validMapping mapping = (`elem` validDigits) . mapDisplay mapping

-- given a list of observed digits, returns the first (only?)
-- mapping that matches all digits.
solveMapping :: [String] -> String
solveMapping observedDigits =
  head (filter validEverywhere (permutations "abcdefg"))
  where
    validEverywhere mapping = all (validMapping mapping) observedDigits

-- given a mapping, and a list of observed segments,
-- interprets them under the mapping and converts to an int.
interpretNumber :: String -> [String] -> Int
interpretNumber mapping = undigits . map getDigit
  where
    getDigit = fromJust . (`elemIndex` validDigits) . mapDisplay mapping

undigits :: [Int] -> Int
-- converts list like [1,2,3] to int 123
undigits = foldl (\t o -> 10 * t + o) 0

-- accepts a list of notes taken,
-- a list of observed digits,
-- and returns the decoded observed digits.
decodeDisplay :: [String] -> [String] -> Int
decodeDisplay notes observed = interpretNumber solvedMapping observed
  where
    solvedMapping = solveMapping (observed ++ notes)

main :: IO ()
main = do
  allInputs <- readInputs
  let solutions = map (uncurry decodeDisplay) allInputs
  print (sum solutions)