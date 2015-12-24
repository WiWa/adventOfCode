import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

-- Very similar to Day 9. Permute and find the max.

type Person = String
type Glee = Int
type Edge = (Person, Person)
type Dict = Map.Map Edge Glee
type Table = [Person]

--https://mail.haskell.org/pipermail/haskell/2006-July/018298.html
perms [] = [[]]
perms xs = [ y : ps | (y,ys) <- selections xs, ps <- perms ys]

selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

readInt :: String -> Int
readInt x = read x :: Int

parseGlee :: (String, String) -> Glee
parseGlee (s, h)
  | s == "gain" = readInt h
  | s == "lose" = readInt ('-':h)

extractInfo :: [String] -> (Person, Person, Glee)
extractInfo str@(a:_:s:h:_) =
  let b = init $ last str
      g = parseGlee (s,h)
  in  (a, b, g)

get :: Edge -> Dict -> Glee
get (a, b) d = fromJust $ Map.lookup (a, b) d

biget (a, b) d = get (a, b) d + get (b, a) d

addInfo :: Dict -> String -> Dict
addInfo mp s =
  let (a, b, g) = extractInfo $ splitOn " " s
  in  Map.insert (a, b) g mp

grokGlee :: Dict -> [String] -> Dict
grokGlee = foldl addInfo

getHappy :: Dict -> Person -> (Table, Glee) -> (Table, Glee)
getHappy d p (t@(h:_), g) = ( p:t, g + biget (p, h) d )
getHappy _ p (t, g) = ( p:t, g )

happyEnding table = biget (head table, last table)

parseTable :: [Person] -> Dict -> Glee
parseTable table d = happyEnding table d + snd (foldr (getHappy d) ([], 0) table)

getAllPeople d = Set.toList $ Set.foldl (\acc (a, b) -> Set.insert b (Set.insert a acc))
          Set.empty (Map.keysSet d)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

maxGlee :: [Person] -> Dict -> Glee
maxGlee people d =
  let tables = perms people
  in  foldl (\glee table -> max glee $ parseTable table d) 0 tables

neutralEdge a b = a ++ " would gain 0 happiness units by sitting next to " ++ b ++ "."

bineutralEdge a b = map (uncurry neutralEdge) [(a, b), (b, a)]

addNeutralEdges me people = concatMap (bineutralEdge me) people

main = do
  print "Day 13!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      relations = grokGlee Map.empty input
      people = getAllPeople relations
      sameppls = iterate (rotate 1) people
      iHaveFriends = addNeutralEdges "Win" people
      newRelations = grokGlee relations iHaveFriends
      newPeople = getAllPeople newRelations
  print people
  -- testing
  print $ take 8 $ map (flip parseTable relations) sameppls
  print $ maxGlee people relations
    -- 664
  print newPeople
  print $ maxGlee newPeople newRelations
    -- 640... oh... okay...
