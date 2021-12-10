import Data.List
import GHC.IO.Handle (isEOF)

data Delimiter = Parenthesis | SquareBracket | CurlyBracket | AngleBracket
  deriving (Eq, Show)

data Token = Opening Delimiter | Closing Delimiter
  deriving (Eq, Show)

-- converts a char to its corresponding token.
untoken :: Char -> Token
untoken '(' = Opening Parenthesis
untoken ')' = Closing Parenthesis
untoken '[' = Opening SquareBracket
untoken ']' = Closing SquareBracket
untoken '{' = Opening CurlyBracket
untoken '}' = Closing CurlyBracket
untoken '<' = Opening AngleBracket
untoken '>' = Closing AngleBracket
untoken _ = undefined

-- stack of currently open chunks.
type ChunkStack = [Delimiter]

-- given a current stack of chunks, and a new token
-- reads the new token and either returns
-- an error containing the erroneous closing delimiter
-- the new chunk stack
readTok :: ChunkStack -> Token -> Either Delimiter ChunkStack
-- always valid to open a new chunk
readTok stack (Opening x) = Right (x : stack)
readTok (b : bs) (Closing x)
  | x == b = Right bs
  | otherwise = Left x
readTok [] (Closing x) = Left x

autocompletePoints :: Delimiter -> Int
autocompletePoints Parenthesis = 1
autocompletePoints SquareBracket = 2
autocompletePoints CurlyBracket = 3
autocompletePoints AngleBracket = 4

-- inner recursive function.
-- accepts an ongoing chunk stack, and a string to process.
computeScore' :: ChunkStack -> String -> Int
-- if the string is empty, then we have an incomplete or well-formed string
-- the completion string is achieved by simply unraveling the stack from its top
computeScore' stack [] = foldl nextScore 0 stack
  where
    nextScore state delim = 5 * state + autocompletePoints delim
-- otherwise, read the token and handle any errors
computeScore' stack (c : cs) = handleErr (readTok stack (untoken c))
  where
    -- no errors? keep processing
    handleErr (Right newStack) = computeScore' newStack cs
    -- corrupted lines yield no autocomplete points
    handleErr (Left Parenthesis) = 0
    handleErr (Left SquareBracket) = 0
    handleErr (Left CurlyBracket) = 0
    handleErr (Left AngleBracket) = 0

-- clean interface to computeScore'
computeScore :: String -> Int
computeScore = computeScore' []

readLines :: IO [String]
readLines = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- readLines
      return (line : rest)

main = do
  lines <- readLines
  let scores = (filter (/= 0) . map computeScore) lines
  let sorted = sort scores
  print (sorted !! (length sorted `div` 2))