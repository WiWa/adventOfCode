import System.Environment
import Data.Maybe (isJust, fromJust)

readInt s = read s :: Integer

-- nonstrict nsum (can be less than n)
nsum _ xs 0 = [Just xs]
nsum _ [] _ = [Nothing]
nsum n (x:xs) target =
  if target < 0 || x > target then [Nothing]
  else
    let inner = if target /= x
                  then map ((:) <$> Just x <*>) $ nsum (n - 1) xs (target - x)
                else [Just [x]]
    in  filter isJust $ inner ++ nsum n xs target

numPts1 = 3
numPts2 = 4
minPkgs1 = 5
minPkgs2 = 4

idealPkgs minPkgs numPts pkgs =
  let leastPkgs = map fromJust $ nsum minPkgs pkgs (quot (sum pkgs) numPts)
  in  foldl (\acc xs -> min acc $ product xs) (product $ head leastPkgs) leastPkgs

idealPkgsPt1 = idealPkgs minPkgs1 numPts1
idealPkgsPt2 = idealPkgs minPkgs2 numPts2

main = do
  print "Day 24!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      pkgs = map readInt input
  print $ idealPkgsPt1 pkgs
  print $ idealPkgsPt2 pkgs
