import Data.List

input = "3113322113"

lenChr :: String -> String
lenChr s = show (length s) ++ [head s]

seeSay :: String -> String
seeSay s =  concatMap lenChr (group s)

-- Pfft, result of "iterate" starts with "input", no "seeSay" applied.
superSay n s = length . last . take (n + 1) $ iterate seeSay s

main :: IO ()
main = do
  print "Day 10!"
  print input
  print $ superSay 40 input
  print $ superSay 50 input
