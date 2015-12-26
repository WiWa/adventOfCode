import System.Environment
import Data.List.Split (splitOn)
import Data.List (intercalate)
import qualified Data.Map as Map

type Prop = (String, Int)
type Aunt = Map.Map String Int

readInt :: String -> Int
readInt x = read x :: Int

criteriaStrs = [
    "children: 3",
    "cats: 7",
    "samoyeds: 2",
    "pomeranians: 3",
    "akitas: 0",
    "vizslas: 0",
    "goldfish: 5",
    "trees: 3",
    "cars: 2",
    "perfumes: 1"
  ]

parseProp :: String -> Prop
parseProp s =
  let kv = splitOn ":" s
  in  (head kv, readInt $ last kv)

criteria = map parseProp criteriaStrs

parseProps s = map parseProp $ splitOn ", " s

addProps :: Aunt-> [Prop] -> Aunt
addProps = foldl (\acc (k, v) -> Map.insert k v acc)

parseAunt s =
  let xs = splitOn ": " s
      count = head xs
      props = intercalate ": " $ tail xs
      num = readInt $ tail $ dropWhile (/= ' ') count
      aunt = Map.insert "Sue" num Map.empty
  in
      addProps aunt $ parseProps props

superMatch f aunt (k, v) = maybe True (f k v) $ Map.lookup k aunt

match :: Aunt -> Prop -> Bool
match aunt (k, v) = superMatch (\_ _ -> (== v)) aunt (k, v)

isAunt :: Aunt -> [Prop] -> Bool
isAunt aunt props = and [ match aunt p | p <- props ]

superFilter k v
  | k `elem` ["cats", "trees"] = (> v)
  | k `elem` ["pomeranians", "goldfish"] = (< v)
  | otherwise = (== v)

superIsAunt :: Aunt -> [Prop] -> Bool
superIsAunt aunt props = and [ superMatch superFilter aunt p | p <- props ]

main = do
  print "Day 16!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      aunts = [ parseAunt aStr | aStr <- input ]
      matches = filter (`isAunt` criteria) aunts
      superMatches = filter (`superIsAunt` criteria) aunts
  print matches
  print superMatches
