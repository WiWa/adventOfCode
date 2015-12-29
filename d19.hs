import System.Environment
import qualified Data.Set as Set
import Data.List
import Data.List.Split (splitOn)
import Data.Char (isUpper)

type Tx = (String, String)
type ToSet = Set.Set String

parseTx s =
  let tx = splitOn " => " s
  in (head tx, head $ tail tx)

makeTx :: String -> Tx -> [String]
makeTx "" _ = []
makeTx s@(x:xs) tx@(from, to)
  | length s < length from = []
  | otherwise =
      let begin = take (length from) s
          rest = drop (length from) s
      in
        if begin == from
          then (to ++ rest) : [ begin ++ restTx | restTx <- makeTx rest tx ]
        else [ x : restTx | restTx <- makeTx xs tx ]

toSet :: String -> [Tx] -> ToSet
toSet _ [] = Set.empty
toSet s (tx:txs) = Set.union (Set.fromList $ makeTx s tx) $ toSet s txs

numUpper = length . filter isUpper

sub (from, to) s = intercalate to $ splitOn from s

count c = length . filter (== c)

countTokens [] _ = 0
countTokens (x:xs) s = count x s + countTokens xs s

-- https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
-- Based askalksi
-- Forreal though wow
part2 s =
  let bs = foldl (flip sub) s [("Rn", "("), ("Ar", ")"), ("Y", ",")]
  in  numUpper s - countTokens ['(', ')'] bs - (2 * count ',' bs) - 1

main = do
  print "Day 19!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      (txStr, rest) = span (isInfixOf "=>") input
      mol = rest!!1
      txs = map parseTx txStr
  print $ Set.size $ toSet mol txs
  -- Part 2 : See http://theburningmonk.com/2015/12/advent-of-code-f-day-19/
  -- Don't think it's particularly worth doing the depth-first-search.
  print $ part2 mol
