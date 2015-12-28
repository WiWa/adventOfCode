import System.Environment
import qualified Data.Set as Set

-- Hello from Day3
-- | Point 2d
type P2 = (Int, Int)
-- | Visited Graph 2d
type VG2 = Set.Set P2

gmin = 0
gmax = 99

xrange = [gmin..gmax]
yrange = xrange

on2on = [2, 3]
off2on = [3]

alwaysOn = [(x,y) | x <- [gmin, gmax], y <- [gmin, gmax] ]

on :: P2-> VG2 -> Bool
on = Set.member

off :: P2-> VG2 -> Bool
off p = not . on p

rangeCheck a = a >= gmin && a <= gmax

check a b x y =
  a /= x || b /= y && rangeCheck a && rangeCheck b

neighbors (x, y) = Set.fromList [ (a, b) | a <- [x-1..x+1], b <- [y-1..y+1],
                                            check a b x y ]

onNeighbors :: P2 -> VG2 -> VG2
onNeighbors p ons = neighbors p `Set.intersection` ons

numOn :: P2 -> VG2 -> Int
numOn p ons = Set.size $ onNeighbors p ons

nextOn :: P2 -> VG2 -> Bool
nextOn p ons
  | p `elem` alwaysOn = True
  | on p ons = numOn p ons `elem` on2on
  | otherwise = numOn p ons `elem` off2on

step :: VG2 -> VG2
step ons = Set.fromList [ (x, y) | x <- xrange, y <- yrange, nextOn (x, y) ons ]

grok :: [String] -> VG2
grok xs = foldl (flip Set.insert) Set.empty $ parseAll xs

range xs = [0..length xs - 1]

parseAll :: [String] -> [P2]
parseAll xs = [ (c, r) | r <- range xs, c <- range (xs!!r),
                          xs!!r!!c == '#' ]

main = do
  print "Day 18!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      initial = foldl (flip Set.insert) (grok input) alwaysOn
      post100 = last $ take 101 $ iterate step initial
  print $ Set.size post100
