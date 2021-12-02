module Main where

import GHC.IO.Handle (isEOF)

-- (horizontal position, depth, aim)
type Position = (Int, Int, Int)

-- Multiplies horizontal position by depth and returns result.
multPositions :: Position -> Int
multPositions (h, d, _) = h * d

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
move (h, d, a) (Forward x) = (h + x, d + (x * a), a)
move (h, d, a) (Up x) = (h, d, a - x)
move (h, d, a) (Down x) = (h, d, a + x)

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
  -- need to foldl; moving is non-commutative
  let finalPos = foldl move (0, 0, 0) instructions
  print (multPositions finalPos)