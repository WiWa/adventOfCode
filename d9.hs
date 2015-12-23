import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust, fromMaybe)
import Data.List.Split (splitOn)

type City = String
type Edge = (City, City)
type Dist = Int
type Graph = Map.Map Edge Dist

dist :: Edge -> Graph -> Dist
dist (a, b) g =
  let d = Map.lookup (a, b) g
  in fromMaybe (fromJust $ Map.lookup (b, a) g) d

insert :: (City, City, String) -> Graph -> Graph
insert (a, b, d) = Map.insert (a, b) (read d :: Int)

parseStr s =
  let (a:to:b:eq:d:_) = splitOn " " s
  in  (a, b, d)

multiDist :: [City] -> Graph -> Dist
multiDist (x:y:z:xs) g = dist (x, y) g + multiDist (y:z:xs) g
multiDist (x:y:xs) g   = dist (x, y) g

setAddTuple (a, b) s = foldl (flip Set.insert) s [a, b]

--https://mail.haskell.org/pipermail/haskell/2006-July/018298.html
perms [] = [[]]
perms xs = [ y : ps | (y,ys) <- selections xs, ps <- perms ys]

selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

main :: IO ()
main = do
  print "Day 9!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      g = foldl (flip insert) Map.empty $ map parseStr input
      cities = Set.toList $
                Map.foldlWithKey (\s k _ -> setAddTuple k s) Set.empty g
      dists = map (`multiDist` g) (perms cities)
      minDist = foldl min maxBound dists
      maxDist = foldl max minBound dists
  print minDist
  print maxDist
