import System.Environment
import Data.Char (chr)
import Debug.Trace (trace)

-- Why do I need to write this myself?
tailN :: Int -> [a] -> [a]
tailN 1 = tail
tailN n = tail . tailN (n - 1)

parseStr acc "" = acc
parseStr acc str@(x:xs) =
  if x == '\\'
    then
      case head xs of
        '\\' -> parseStr (acc ++ "\\")  (tailN 2 str)
        '\"' -> parseStr (acc ++ "\"")  (tailN 2 str)
        'x'  -> parseStr (acc ++ "X") (tailN 4 str)
  else parseStr (acc ++ [x]) (tail str)

inMemLength s = length (parseStr "" s) - 1

stringLength s = length s + 1

seeString ('\"':s) = (stringLength s, inMemLength s)

accSeeString (a, b) s =
  let (x, y) = seeString s
  in  (a + x, b + y)

encodeStr acc "" = "\"" ++ acc ++ "\""
encodeStr acc (x:xs) =
  case x of
    '\\' -> encodeStr (acc ++ "\\\\")  xs
    '\"' -> encodeStr (acc ++ "\\\"")  xs
    _    -> encodeStr (acc ++ [x]) xs


seeString2 s = (length s, length $ encodeStr "" s)

accSeeString2 (a, b) s =
  let (x, y) = seeString2 s
  in  (a + x, b + y)

main :: IO ()
main = do
  print "Day 8!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      (a, b) = foldl accSeeString (0,0) input
      (c, d) = foldl accSeeString2 (0, 0) input
  print $ a - b
  print $ d - c
