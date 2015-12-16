import System.Environment
import Data.List

sublistsN :: Int -> [t] -> [[t]]
sublistsN _ [] = []
sublistsN n xss@(_:xs)
  | n <= length xss = take n xss : sublistsN n xs
  | otherwise = []

vowels :: String
vowels = "aeiou"

censored :: [String]
censored = ["ab", "cd", "pq", "xy"]

(#) :: a -> (a -> b) -> b
infixr 0 #
f # g = g f

niceVowels :: String -> Bool
niceVowels s = length (s `intersect` vowels) # (>= 3)

niceDup :: String -> Bool
niceDup s = (not . null) $ filter (\xs -> length xs > 1) $ group s

niceCensor :: String -> Bool
niceCensor s = null [ x | x <- censored, x `isInfixOf` s]

type Criterion = [String -> Bool]

criterion1 :: Criterion
criterion1 = [niceVowels, niceDup, niceCensor]

niceDoublePair :: String -> Bool
niceDoublePair (x:y:xs) = [x, y] `isInfixOf` xs || niceDoublePair (y:xs)
niceDoublePair _ = False

niceTriple :: String -> Bool
niceTriple s = (not . null) $ filter triple $ sublistsN 3 s

triple :: (Eq t) => [t] -> Bool
triple (a:_:c:_) = a == c
triple _ = False

criterion2 :: Criterion
criterion2 = [niceDoublePair, niceTriple]

niceWord :: Criterion -> String -> Bool
niceWord criterion s = foldl (\acc f -> acc && f s) True criterion

countNice :: Criterion -> [String] -> Int
countNice criterion xs = length $ filter (niceWord criterion) xs

main :: IO ()
main = do
  print "Day 5!"

  args <- getArgs
  info <- readFile $ head args
  print $ countNice criterion1 $ lines info
  print $ countNice criterion2 $ lines info
