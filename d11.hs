import Data.List
import Data.Char

input = "vzbxkghb"

-- Validator

consecSubseq (a:b:c:xs) =
  (ord b - ord a == 1 && ord c - ord b == 1)
    || consecSubseq (b:c:xs)
consecSubseq _ = False

noBadLetters s =
  let badLetters = ['i', 'o', 'l']
  in null $ badLetters `intersect` s

multiPairs :: String -> Bool
multiPairs s =
  ((>1) $ length $ filter ((>1) . length) $ group s)
  ||  any ((>3) . length) (group s)

validators = [noBadLetters, consecSubseq, multiPairs]
validPass s = foldl (\v f -> v && f s) True validators

-- Incrementer

increChr c =
  if c == 'z'
    then 'a'
  else chr $ ord c + 1

increStr s =
  let next = increChr (last s)
      prefix = init s
  in
    if next == 'a'
      then increStr prefix ++ [next]
    else prefix ++ [next]

nextValidPass s = head $ dropWhile (not.validPass) $ tail $ iterate increStr s

main = do
  print "Day 11!"
  print $ take 3 $ iterate nextValidPass input
