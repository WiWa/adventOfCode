import System.Environment

total = 150

readInt :: String -> Int
readInt x = read x :: Int

-- assumes positive nonzero elems
sumChoose :: [Int] -> Int -> [[Int]]
sumChoose [] _ = [[]]
sumChoose xs n
  | n <= 0 = [[]]
  | otherwise =
    let splits = [ (last a, b ) | (a, b) <- [ splitAt i xs | i <- [1..length xs] ] ]
    in [ x : ys | (x, rest) <- splits, ys <- sumChoose rest (n - x), x + sum ys == n]

minLen xs = foldl min maxBound $ map length xs

filterLen n = filter ((n ==) . length)

main = do
  print "Day 16!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      containers = map readInt input
      combos = containers `sumChoose` total
  print $ length combos
  print $ length $ filterLen (minLen combos) combos
