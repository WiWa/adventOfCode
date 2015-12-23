import Text.Regex.Posix
import System.Environment

numMatch = "(-?[0-9]+)"

numRead x = read x :: Int

main = do
  print "Day 12!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
  print $ sum $ map numRead (getAllTextMatches $ content =~ numMatch :: [String])
