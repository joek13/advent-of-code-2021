module Main where

import Data.Maybe
import System.IO

countLines ::
  -- | the value of the last line, if available
  Maybe Int ->
  IO Int
countLines lastLine = do
  -- check if we have hit end of stream
  done <- isEOF
  if done
    then -- if so, just return 0
      return 0
    else do
      num <- readLn -- read in line, parse as Int
      if isJust lastLine && (num > fromJust lastLine)
        then do
          nextLines <- countLines (Just num)
          return (1 + nextLines)
        else {-
             Count the next n-1 lines, using the current  as the "last" line.
             -}
          countLines (Just num)

main = do
  increases <- countLines Nothing
  print increases