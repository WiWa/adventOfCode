import System.Environment
import Debug.Trace (trace)

type Reg = (Char, Int)
type Regs = [Reg]
type Op = Regs -> String -> Int -> (Regs, Int)

ops = [
    ("hlf", hlf),
    ("tpl", tpl),
    ("inc", inc),
    ("jmp", jmp),
    ("jie", jie),
    ("jio", jio)
  ]

filterFst first = filter ((== first) . fst)

getReg :: Regs -> String -> Int
getReg rs (r:_) = (snd . head) $ filterFst r rs

setReg :: Regs -> String -> Int -> Regs
setReg rs (r:_) v = filter (\(a,_) -> a /= r) rs ++ [(r, v)]

getOp :: String -> Op
getOp opname = (snd . head) $ filterFst opname ops

parseInt s =
  let clean = if head s == '+' then tail s else s
  in  read clean :: Int

-- func regs reg params cursor
hlf rs r c = (setReg rs r $ quot (getReg rs r) 2 , c + 1)
tpl rs r c = (setReg rs r $ getReg rs r * 3 , c + 1)
inc rs r c = (setReg rs r $ getReg rs r + 1 , c + 1)
jmp :: Op
jmp rs off c = let x = c + parseInt off in (rs, x)
jif :: (Int -> Bool) -> Op
jif f rs rOff c =
  let (r, off_) = span (/= ',') rOff
      off = tail $ tail off_
  in  if f (getReg rs r) then jmp rs off c else (rs, c + 1)
jie = jif even
jio = jif (== 1)

parseOps :: Regs -> [String] -> Int -> Regs
parseOps rs os c =
  if c >= length os
    then rs
  else
    let (opname, param_) = span (/= ' ') $ os!!c
        param = tail param_
        (nextRs, nextC) = getOp opname rs param c
    in
        parseOps nextRs os nextC

main = do
  print "Day 23!"
  args <- getArgs
  contents <- readFile $ head args
  let input = lines contents
      startRegsPt1 = [('a', 0), ('b', 0)]
      startRegsPt2 = [('a', 1), ('b', 0)]
  print $ length input
  print $ parseOps startRegsPt1 input 0
  print $ parseOps startRegsPt2 input 0
