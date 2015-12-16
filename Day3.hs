import System.Environment
import Data.List

-- | Point 2d
type P2 = (Int, Int)
-- | Visited Graph 2d
type VG2 = [P2]

unzipList :: [a] -> ([a], [a]) -> Bool -> ([a], [a])
unzipList (x:xs) (a, b) True = unzipList xs (x:a, b) False
unzipList (x:xs) (a, b) False = unzipList xs (a, x:b) True
unzipList _ (a, b) _ = (reverse a, reverse b)

visitPoint :: VG2 -> P2 -> VG2
visitPoint vg p = p:vg

nextPoint :: P2 -> Char -> P2
nextPoint (x, y) '^' = (x, y + 1)
nextPoint (x, y) 'v' = (x, y - 1)
nextPoint (x, y) '>' = (x + 1, y)
nextPoint (x, y) '<' = (x - 1, y)
nextPoint _ _ = (0, 0)

numVisited :: [String] -> IO Int
numVisited infos = do
  let visited = map allVisited infos
      flattened = concat visited
  return $ length $ nub flattened

allVisited :: String -> VG2
allVisited = foldl (\acc@(a:_) c -> visitPoint acc $ nextPoint a c) [(0, 0)]

main :: IO ()
main = do
  print "Starting!"
  args <- getArgs
  print args

  info <- readFile $ head args
  let ab = unzipList info ("", "") True
  x <- numVisited [info]
  print $ "1st: " ++ show x
  y <- numVisited [fst ab, snd ab]
  print $ "2nd: " ++ show y
