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

-- inner recursive function.
-- accepts an ongoing chunk stack, and a string to process.
computeScore' :: ChunkStack -> String -> Int
-- if the string is empty, then we have an incomplete or well-formed string
computeScore' stack [] = 0
-- otherwise, read the token and handle any errors
computeScore' stack (c : cs) = handleErr (readTok stack (untoken c))
  where
    -- no errors? keep processing
    handleErr (Right newStack) = computeScore' newStack cs
    handleErr (Left Parenthesis) = 3
    handleErr (Left SquareBracket) = 57
    handleErr (Left CurlyBracket) = 1197
    handleErr (Left AngleBracket) = 25137

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
  let scores = map computeScore lines
  print (sum scores)