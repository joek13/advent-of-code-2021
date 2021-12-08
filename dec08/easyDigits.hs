import Data.Text (pack, splitOn, unpack)
import GHC.IO.Handle (isEOF)

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

main :: IO ()
main = do
  inputs <- readInputs
  let displayOutputs = map snd inputs
  let isEasy = (`elem` [2, 4, 3, 7]) . length
  print (sum (map (length . filter isEasy) displayOutputs))