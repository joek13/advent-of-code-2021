module Main where

import GHC.IO.Handle (isEOF)

-- (horizontal position, depth)
type Position = (Int, Int)

-- instruction
data Instruction = Forward Int | Down Int | Up Int
  deriving (Show)

-- derive parsing for Instruction type
instance Read Instruction where
  readsPrec _ str
    | direction == "forward" = [(Forward by, "")]
    | direction == "down" = [(Down by, "")]
    | direction == "up" = [(Up by, "")]
    | otherwise = []
    where
      split = words str -- split input by space
      direction = head split -- get string before space
      by = read (head (tail split)) -- get number after space, read as Int

-- accepts a position and an instruction. executes the instruction,
-- returning a new position
move :: Position -> Instruction -> Position
move (h, d) (Forward n) = (h + n, d)
move (h, d) (Up n) = (h, d - n)
move (h, d) (Down n) = (h, d + n)

readInstructions :: IO [Instruction]
readInstructions = do
  done <- isEOF
  if done
    then return []
    else do
      instruction <- readLn
      later <- readInstructions
      return (instruction : later)

main :: IO ()
main = do
  instructions <- readInstructions
  let finalPosition = foldl move (0, 0) instructions
  print (uncurry (*) finalPosition)