import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS

input :: String
input = "yzbqklnj"

rightPrefix :: Int -> String -> Bool
rightPrefix n str = take n str == replicate n '0'

makeMD5s :: String -> [(String, Int)]
makeMD5s base = [ (toMD5 $ base ++ show i, i) | i <- [1..] ]

firstMD5 :: String -> (String, Int)
firstMD5 base = head $ filter (rightPrefix 5 . fst) $ makeMD5s base

secondMD5 :: String -> (String, Int)
secondMD5 base = head $ filter (rightPrefix 6 . fst) $ makeMD5s base

toMD5 :: String -> String
toMD5 x = show $ md5 $ BS.pack x

main :: IO ()
main = do
  print "Day 4!"

  print $ toMD5 input
  print $ firstMD5 input
  print $ secondMD5 input
