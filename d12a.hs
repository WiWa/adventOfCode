{-# LANGUAGE FlexibleContexts #-}

import Text.Regex.Posix
import System.Environment
import Debug.Trace (trace)
import Data.List

{-
  It appears to me much more difficult than the first part.
  Ignore any object with the value "red" and its subparts.

  We can build a string with these objects removed.
  Imagine a cursor traversing our JSON string.
  String A is everything after the cursor.

  JK. Try this:
  The total sum is the sum of partial sums
  A partial sum is made of numbers preceding a "{"
  So: "[1,2,3, {...}...]" would give us a string and an int:
  "{...}...]" and 6.
  Use a stack to parse the "{...}" via a takewhile or something.
  Regex inside the "{...}" for /:"red"/.
    Except this would exclude an outer object if an inner object has red.
    In that case, we can simply chunk-read again,
    but we'll have to return 0 if we see /:"red"/

    The problem with chunk-reading only the left side of a "{...}"
      is that if we find a /:"red"/ in the right side, we're screwed.
    So we need to extract objects. Chunk by (outer, inner).

    Problem was [ {} {} ]. Gdi.
-}

numMatch = "(-?[0-9]+)"

numRead x = read x :: Int

badProp = ":\"red\""

hasStr s r = s =~ r :: Bool

numSum s = sum $ map numRead (getAllTextMatches $ s =~ numMatch :: [String])

addLast :: [[a]] -> a -> [[a]]
addLast xs c = init xs ++ [ last xs ++ [c] ]

stackPartitioner :: (String, [String], String) -> Char -> (String, [String], String)
stackPartitioner (o, is, s) c
  | c == '{'  = if null s then (o, is ++ [""], '{':s)
                  else (o, addLast is c, '{':s)
  | c == '}'  = if null $ tail s then (o, is ++ [""], tail s)
                  else (o, addLast is c, tail s)
  | null s    = (o ++ [c], is, s)
  | otherwise = (o, addLast is c, s)

outerInners :: String -> (String, [String])
outerInners s =
  let (o, is, _) = foldl stackPartitioner ("", [], "") s
  in  (o, is)

chunkSum "" = 0
chunkSum s =
  let (outer, inners) = outerInners s
      osum = numSum outer
      bad = hasStr outer badProp
  in
    if null inners
      then if bad then 0 else osum
    else
      let isum = sum $ map chunkSum inners
      in if bad then 0 else osum + isum

main = do
  print "Day 12!"
  args <- getArgs
  content <- readFile $ head args
  print $ numSum content
  print $ chunkSum content
