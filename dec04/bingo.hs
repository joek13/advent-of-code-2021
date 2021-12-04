{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List
import Data.Traversable (for)
import GHC.IO.Handle (isEOF)

-- type of empty boards
type Board = [[Int]]

-- type of boards in play; int together
-- with bool for whether it is marked yet
type PlayBoard = [[(Int, Bool)]]

-- Makes a playboard from an empty board.
mkPlayBoard :: Board -> PlayBoard
mkPlayBoard = map (map (,False))

-- Marks a playboard with a new number.
markPlayBoard :: Int -> PlayBoard -> PlayBoard
markPlayBoard num = map (map mark)
  where
    mark (val, marked)
      | val == num = (val, True)
      | otherwise = (val, marked)

-- whether a PlayBoard is winning.
winning :: PlayBoard -> Bool
winning board =
  any rowWins marks -- any winning rows
    || any rowWins (transpose marks) -- any winning columns
  where
    -- select just whether a board is marked
    marks = map (map snd) board
    rowWins = and -- whether a row wins is just the boolean AND of each cell

computeScore :: Int -> PlayBoard -> Int
-- compute sum of unmarked cells, multiply by last call
computeScore lastCall winningBoard = sum (map fst unmarked) * lastCall
  where
    flattened = concat winningBoard -- flatten the winning board
    unmarked = filter (not . snd) flattened -- select only unmarked cells

-- Simulates a game of bingo, and returns the score of the winner.
simulateFirstWin :: [Int] -> [PlayBoard] -> Int
-- check if the next call makes a winner
simulateFirstWin (call : calls) boards
  -- if so, compute score for the first winner
  | any winning nextBoards = computeScore call (head (filter winning nextBoards))
  -- otherwise, simulateFirstWin the game further
  | otherwise = simulateFirstWin calls nextBoards
  where
    nextBoards = map (markPlayBoard call) boards
-- game ended without a winner
simulateFirstWin [] _ = undefined

simulateLastWin :: [Int] -> [PlayBoard] -> Int
-- only one board left, it must be the last winner
simulateLastWin (call : calls) [lastBoard]
  -- simulate until it wins
  | winning marked = computeScore call marked
  -- hasn't won, keep going
  | otherwise = simulateLastWin calls [marked]
  where
    marked = markPlayBoard call lastBoard
-- still multiple boards in play, keep simulating
-- and filter out winners at each step
simulateLastWin (call : calls) boards = simulateLastWin calls notWinningBoards
  where
    notWinningBoards = filter (not . winning) (map (markPlayBoard call) boards)
-- game ended without a winner
simulateLastWin [] _ = undefined

-- Splits a string into a list of strings, delimited by the token.
-- Thanks https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitBy :: Char -> String -> [String]
splitBy token str
  | left == "" = []
  | otherwise = w : splitBy token remainder
  where
    left = dropWhile (== token) str
    (w, remainder) = break (== token) left

-- reads sequence of numbers to call from stdin.
readSequence :: IO [Int]
readSequence = do
  line <- getLine
  let strValues = splitBy ',' line
  return (map read strValues)

-- reads board from stdin.
readBoard :: IO Board
readBoard =
  for
    [1 .. 5]
    ( \_ -> do
        map read . words <$> getLine
    )

-- reads boards from stdin until EOF.
readBoards :: IO [Board]
readBoards = do
  done <- isEOF
  if done
    then return []
    else do
      _ <- getLine -- skip empty line before board
      board <- readBoard -- read this board
      rest <- readBoards -- read future boards
      return (board : rest) -- concatenate

main :: IO ()
main = do
  seq <- readSequence
  boards <- readBoards
  let playboards = map mkPlayBoard boards
  print (simulateLastWin seq playboards)