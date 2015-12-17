
import System.Environment
-- What is Data Structures
import Data.Map.Lazy (Map)
import qualified Data.Set as Set
import Data.List.Split

type P2 = (Int, Int)
type VG2 = Set.Set P2
-- | {x: {y: v} }
type PtMap = Map Int (Map Int Int)

through :: P2 -> P2 -> VG2
through (a,b) (c,d) = foldr Set.insert Set.empty
                        [(x,y) | x <- [a..c], y <- [b..d]]

getPoint (a,b) = head [ p | p@(x,y,z) <- allPoints, (x,y) == (a,b) ]

allPoints :: VG2
allPoints = through (0,0) (999,999)

parseCmd :: String -> VG2
parseCmd "turn on" ps = vg `Set.union` ps
parseCmd "turn off" ps = vg `Set.difference` ps
parseCmd "toggle" ps = foldl (parseCmd "turn on") ps (replicate 2 0)

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
    print "Day 6a!"
