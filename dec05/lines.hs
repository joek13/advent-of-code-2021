import System.IO (isEOF)

data LineSegment = LineSegment {startpoint :: (Int, Int), endpoint :: (Int, Int)}

-- converts string "i,j" to int-pair (i, j)
readPair :: String -> (Int, Int)
readPair str = (left, right)
  where
    -- get part of string before the ','
    leftStr = takeWhile (/= ',') str
    -- read as int
    left = read leftStr
    -- get part of string after the ',', read as int
    right = read (drop (length leftStr + 1) str)

-- reads in a single segment
readSegment :: IO LineSegment
readSegment = do
  line <- getLine
  -- line format: (x1,y1) -> (x2,y2)
  let split = words line -- splitting by spaces yields [(x1,y1),->,(x2,y2)]
  let startpoint = readPair (head split) -- read pair from first word
  let endpoint = readPair (split !! 2) -- and from third word
  return (LineSegment startpoint endpoint)

readSegments :: IO [LineSegment]
readSegments = do
  done <- isEOF
  if done
    then return []
    else do
      segment <- readSegment
      rest <- readSegments
      return (segment : rest)

allEndpoints :: [LineSegment] -> [(Int, Int)]
allEndpoints = concatMap (\x -> [startpoint x, endpoint x])

-- checks whether the second argument is within the first interval, inclusive
-- ignores the order of the upper/lower bound
between :: (Int, Int) -> Int -> Bool
between interval val = lower <= val && val <= upper
  where
    lower = uncurry min interval
    upper = uncurry max interval

isIncident :: LineSegment -> (Int, Int) -> Bool
isIncident (LineSegment (x1, y1) (x2, y2)) (x, y)
  -- vertical line
  | x1 == x2 = (x == x1) && between (y1, y2) y
  -- horizontal line
  | y1 == y2 = (y == y1) && between (x1, x2) x
  -- don't handle other cases
  | otherwise = False

-- adds "1" to every incident point in the grid
plotLine :: [[Int]] -> LineSegment -> [[Int]]
plotLine mat line =
  [ [ mat !! row !! col + if isIncident line (row, col) then 1 else 0
      | col <- [0 .. (width - 1)]
    ]
    | row <- [0 .. (height - 1)]
  ]
  where
    height = length mat
    width = length (head mat)

main :: IO ()
main = do
  segments <- readSegments
  let maxRow = maximum (map fst (allEndpoints segments))
  let maxCol = maximum (map snd (allEndpoints segments))

  let grid = [[length (filter (`isIncident` (i, j)) segments) | j <- [0 .. maxCol]] | i <- [0 .. maxRow]]
  print (sum (map (length . filter (>= 2)) grid))