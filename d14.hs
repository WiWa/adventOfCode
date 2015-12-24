import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

timeLimit = 2503

data Deer = Deer {
    name    :: String
  , speed   :: Int
  , time    :: Int
  , rest    :: Int
}

instance Show Deer where
  show = name

readInt x = read x :: Int

makeDeer :: [String] -> Deer
makeDeer (n:can:fly:speed:kms:for:time:xs) =
  let s = readInt speed
      t = readInt time
      r = readInt (last $ init xs)
  in  Deer n s t r

makeDeers :: [String] -> [Deer]
makeDeers = map (makeDeer . splitOn " ")

kmRun deer@Deer{speed = speed, time = time, rest = rest} t =
  let runRests = takeWhile (>=0) $ tail $ iterate (\x -> x - time - rest) t
      leftover = if null runRests then t else last runRests
      runtime = min time leftover
  in
    speed * (time * length runRests  + runtime)

maxKm deers t = foldl max minBound $ map (flip kmRun t) deers

maxDeers deers t =
  let mxkm = maxKm deers t
  in  filter ((mxkm ==) . flip kmRun t) deers

addPoint :: Map.Map String Int -> Deer -> Map.Map String Int
addPoint mp deer =
  let d = name deer
  in
    if Map.member d mp
      then Map.adjust (+1) d mp
    else Map.insert d 1 mp

countPoints :: [Deer] -> Map.Map String Int
countPoints deers = foldl addPoint Map.empty deers

points :: [Deer] -> Int -> Map.Map String Int
points deers t = countPoints $ concatMap (maxDeers deers) [1..t]

maxSnd (a, b) (c, d)
  | b >= d    = (a, b)
  | otherwise = (c, d)

maxDeer :: [Deer] -> Int -> (String, Int)
maxDeer deers t = foldl maxSnd ("", minBound) $ Map.toList $ points deers t

main = do
  print "Day 14!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      deers = makeDeers input
  print deers
  print $ maxKm deers timeLimit
  print $ maxDeer deers timeLimit
