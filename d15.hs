import System.Environment
import Text.Regex.Posix

maxTs = 100
part2cal = 500

numMatch = "(-?[0-9]+)"

numRead x = read x :: Int

data Ingredient = Ingredient {
    name  :: String
  , cap   :: Int
  , dur   :: Int
  , fla   :: Int
  , tex   :: Int
  , cal   :: Int
} deriving (Show)

score Ingredient{cap = cap, dur = dur, fla = fla, tex = tex} =
  cap * dur * fla * tex

base = Ingredient "Cookie"  0 0 0 0 0

combine Ingredient{name = aname, cap = acap, dur = adur, fla = afla, tex = atex, cal = acal}
    Ingredient{cap = bcap, dur = bdur, fla = bfla, tex = btex, cal = bcal} =
      let ccap = acap + bcap
          cdur = adur + bdur
          cfla = afla + bfla
          ctex = atex + btex
          ccal = acal + bcal
      in  Ingredient aname ccap cdur cfla ctex ccal

tuplify5 (a:b:c:d:e:_) = (a,b,c,d,e)
uncurry5 f (a, b, c, d, e) = f a b c d e

easymake name = uncurry5 (Ingredient name) . tuplify5

multiple n Ingredient{name = name, cap = cap, dur = dur, fla = fla, tex = tex, cal = cal} =
  easymake name $ map (n*) [cap, dur, fla, tex, cal]

base0 Ingredient{name = name, cap = cap, dur = dur, fla = fla, tex = tex, cal = cal} =
  easymake name $ map (max 0) [cap, dur, fla, tex, cal]

blend :: [Ingredient] -> Ingredient
blend = foldl combine base

-- Results in m^(n-1) + 1 lists
-- Wait, what, no it doesn't??????
-- m = 2, then: 1 3 6 10 15 21 28 ... perfect numbers
-- m = 3, then: 1 4 10 20 35 56 ... difference is the perfect numbers
allRatios :: Int -> Int -> [[Int]]
allRatios m 1 = [[m]]
allRatios m n = [ x : ys | x <- [0..m], ys <- allRatios (m - x) (n - 1) ]

mix :: [Ingredient] -> [Int] -> Ingredient
mix xs ratio = base0 $ blend $ map (\(i,r) -> multiple r i) $ zip xs ratio

parseStr s =
  let iname = takeWhile (/= ':') s
      props = map numRead (getAllTextMatches $ s =~ numMatch :: [String])
  in  easymake iname props

allCookies is m = [ mix is ratio | ratio <- allRatios m (length is)]

maxScoreFilter f is m = foldl max 0 [ score x | x <- allCookies is m, f x]
maxScore = maxScoreFilter (const True)
maxScoreCal c = maxScoreFilter (\x -> cal x == c)

main = do
  print "Day 15!"
  args <- getArgs
  content <- readFile $ head args
  let input = lines content
      is = map parseStr input
  print is
  print $ maxScore is maxTs
  print $ maxScoreCal part2cal is maxTs
