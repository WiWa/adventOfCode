
import System.Environment
-- What is Data Structures
import qualified Data.Map.Lazy as Map
-- import qualified Data.Set as Set
import Data.List.Split
-- import Data.Maybe (isJust, fromJust)

type P2 = (Int, Int)
type VG2 = [P2]
-- | { (x,y): v } }
type PtMap = Map.Map P2 Int

through :: P2 -> P2 -> VG2
through (a,b) (c,d) = [(x,y) | x <- [a..c], y <- [b..d]]

parseCmdPt :: PtMap -> P2 -> Int -> String -> PtMap
parseCmdPt mp pt val "turn on" = Map.adjust (\_ -> val + 1) pt mp
parseCmdPt mp pt 1 "turn off" = Map.delete pt mp
parseCmdPt mp pt 0 "turn off" = Map.delete pt mp
parseCmdPt mp pt val "turn off" = Map.adjust (\_ -> val - 1) pt mp
parseCmdPt mp pt val "toggle" = Map.adjust (\_ -> val + 2) pt mp
parseCmdPt mp _ _ _ = mp

getCmdFunc :: PtMap -> P2 -> String -> PtMap
getCmdFunc mp pt =
  let xy = Map.lookup pt mp
  in maybe (parseCmdPt (addPt mp pt) pt 0) (parseCmdPt mp pt) xy

addPt :: PtMap -> P2 -> PtMap
addPt mp pt = Map.insert pt 0 mp

parseCmd :: String -> PtMap -> VG2 -> PtMap
parseCmd cmd = foldl (\accMp pt -> getCmdFunc accMp pt cmd)

commands = ["turn on", "turn off", "toggle"]

extractCmd s = head [ c | c <- commands, c == take (length c) s]

extractPts s = makePts $ reverse $ take 3 $ reverse $ splitOn " " s
    where   makePts (x:_:y:_)   = (parsePt x, parsePt y)
            makePts _           = ((0,0), (0,0))
            parsePt str =   let (x:y:_) = splitOn "," str
                            in (read x, read y) :: P2

makeCmd :: String -> PtMap -> PtMap
makeCmd s mp = let (x, y) = extractPts s
            in  parseCmd (extractCmd s) mp $ x `through` y

grokCmds :: [String] -> PtMap
grokCmds = foldl (flip makeCmd) Map.empty

main = do
    print "Day 6a!"
    -- print $ extractPts "toggle 0,0 through 1,1"
    -- print $ (0,0) `through` (1,1)
    -- print $ parseCmd "toggle" Map.empty $ (0,0) `through` (1,1)
    -- let a = makeCmd "toggle 0,0 through 500,500" Map.empty
    --     b = makeCmd "turn off 255,255 through 350,360" a
    --     c = makeCmd "turn on 100,0 through 299,299" b
    --     d = Map.filter (> 1) c
    -- -- print a
    -- -- print b
    -- -- print c
    -- -- print d
    -- print $ Map.size d
      --242850
    args <- getArgs
    info <- readFile $ head args
    -- let mp = Map.filter (>0) $ grokCmds $ take 100 $ lines info
    -- print $ Map.size mp
    -- 798120
    print $ Map.foldl (+) 0 $ grokCmds $ lines info
    -- 15343601
      -- 97% of 16GB memory lmao
      -- Idea: Keep a counting sum.
      -- After all, with these new rules, the sum is simply a superposition of sums.
      -- I lied, minimum of zero.
