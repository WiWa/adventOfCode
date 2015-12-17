-- TODO Maybe-ify?
import System.Environment
-- What is Data Structures
import qualified Data.Set as Set
import Data.List.Split

-- Hello from Day3
-- | Point 2d
type P2 = (Int, Int)
-- | Visited Graph 2d
type VG2 = Set.Set P2

through :: P2 -> P2 -> Set.Set P2
through (a,b) (c,d) = foldr Set.insert Set.empty
                        [(x,y) | x <- [a..c], y <- [b..d]]

allPoints :: VG2
allPoints = through (0,0) (999,999)

parseCmd :: String -> VG2 -> VG2 -> VG2
parseCmd "turn on" vg ps = vg `Set.union` ps
parseCmd "turn off" vg ps = vg `Set.difference` ps
parseCmd "toggle" vg ps = (vg `Set.difference` ps) `Set.union` (ps `Set.difference` vg)

commands = ["turn on", "turn off", "toggle"]

extractCmd s = head [ c | c <- commands, c == take (length c) s]

extractPts s = makePts $ reverse $ take 3 $ reverse $ splitOn " " s
    where   makePts (x:_:y:_)   = (parsePt x, parsePt y)
            makePts _           = ((0,0), (0,0))
            parsePt str =   let (x:y:_) = splitOn "," str
                            in (read x, read y) :: P2

makeCmd s vg = let (x, y) = extractPts s
            in  parseCmd (extractCmd s) vg $ x `through` y

grokCmds :: [String] -> VG2
grokCmds = foldl (flip makeCmd) Set.empty

main = do
    print "Day 6!"
    args <- getArgs
    info <- readFile $ head args
    let ps = grokCmds $ lines info
    print $ Set.size ps
