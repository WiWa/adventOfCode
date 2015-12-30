import Data.List
import Data.Function (on)
import Debug.Trace (trace)
-- Brute Force, starting with Rec Poi Shield.
-- (Cost, Damage, MP, HP)
type Turn = Int
type BossHp = Int
type Fight = (Turn, Player, BossHp)

type Effect = (String, Spell, Int)
data Player = Player{
    hp    :: Int
  , mp    :: Int
  , tcst  :: Int
  , tdmg  :: Int
  , fx    :: [Effect]
} deriving (Show)
data Spell = Spell{
    name  :: String
  , cst   :: Int
  , dmg   :: Int
  , dhp   :: Int
  , dmp   :: Int
  , sfx   :: [Effect]
}
instance Show Spell where
  show = name

type Plan = [Spell]

bossHp = 58
bossDmg = 9

total :: (Spell -> Int) -> Plan -> Int
total f plan = sum $ map f plan

sortCost plans = sortBy (compare `on` total cst) plans

magStr = "mag"
draStr = "dra"
shiStr = "shi"
poiStr = "poi"
recStr = "rec"
mag = Spell magStr 53 4 0 0 []
dra = Spell draStr 73 2 2 0 []
shi = Spell shiStr 113 0 0 0 [(shiStr, noop, 6)]
poi = Spell poiStr 173 0 0 0 [(poiStr, poiTick, 6)]
poiTick = Spell poiStr 0 3 0 0 []
rec = Spell recStr 229 0 0 0 [(recStr, recTick, 5)]
recTick = Spell recStr 0 0 0 101 []
hardModeTick = Spell "hm" 0 0 (-1) 0 []
noop = Spell "noop" 0 0 0 0 []

inPlan :: Spell -> Plan -> Bool
inPlan s p = name s `elem` map name p

getFx :: String -> Effect
getFx s = head $ sfx (getSpell s)

getTick "hm" = hardModeTick
getTick s = let (_,fxTick,_) = getFx s in fxTick

getSpell s = head $ filter ((s == ).name) spells

spells = [mag, dra, shi, poi, rec]

minMp = foldl min maxBound $ map dmp spells

player = Player 50 500 0 0 []

norec :: Plan -> Bool
norec plan = not $ inPlan rec $ take 2 $ reverse plan

newPlans :: Plan -> Player -> [Plan]
newPlans plan pl
  | mp pl <= minMp && norec plan = [plan ++ [rec]]
  | otherwise = map (\s -> plan ++ [s]) spells

matchTurn s t
  | s == poiStr = True
  | s == shiStr = even t
  | s == recStr = True
  | s == "hm" = myTurn t

applyFx :: Turn -> Player -> String -> Player
applyFx t p@Player{hp = hp, mp = mp, tcst = tcst, tdmg = tdmg, fx = fx} fxname =
  let
      xs = [poiStr, shiStr, recStr, "hm"]
      spfx = filter (== fxname) xs
  in
      if (not.null) spfx && matchTurn fxname t
        then p `cast` getTick fxname
      else p

applyEffects t p@Player{hp = hp, mp = mp, tcst = tcst} =
  let fxs = map (\(name,_,_) -> name) $ fx p
  in reduceEffects $ foldl (applyFx t) p fxs

reduceEffects p@Player{fx = fx} =
  let rfx = filter (\(_,_, i) -> i > 0) $ map (\(name,f, i) -> (name,f, i - 1) ) fx
  in  p{fx = rfx}

myTurn = odd

cast p@Player{hp = hp, mp = mp, tcst = tcst, tdmg = tdmg, fx = fx} sp =
  let nhp = hp + dhp sp
      nmp = mp + dmp sp - cst sp
      ncst = tcst + cst sp
      ndmg = tdmg + dmg sp
      nfx = fx ++ sfx sp
  in Player nhp nmp ncst ndmg nfx

hasFx p@Player{fx = fx} spell = any (\(str,_,_) -> str == name spell) fx

getPwnd p@Player{hp = hp, fx = fx} =
  if p `hasFx` shi
    then p {hp = hp - max 1 (bossDmg - 7)}
  else p {hp = hp - bossDmg}

validState p@Player{hp = hp, mp = mp, tcst = tcst} =
  hp > 0 &&
  mp > 0 &&
  tcst < 1500

doTurn :: Fight -> Spell -> Fight
doTurn invalid@(0,_,_) _ = invalid
doTurn (turn, p, bhp) spell =
  let (nt, np) = (turn + 1, applyEffects turn p)
      nnp = if myTurn turn then np `cast` spell else getPwnd np
      next = (nt, nnp, bhp)
  in
    -- trace (show (turn, p, name spell)) $
    if validState nnp then next else doTurn (0, nnp, 0) spell

superTurn :: (Fight, Plan) -> Spell -> (Fight, Plan)
superTurn (f, p) s =
  let next@(t,_,_) = doTurn f s
  in  (next, p ++ [s])

validFight (t,_,_) = t >= 0
validPlan plan = let (a,_,_) = simPlan plan in a > 0

simPlan :: Plan -> Fight
simPlan plan = simPlanP plan player

simPlanP plan p =
  let base = (1, p, bossHp)
  in  foldl doTurn base (intersperse noop plan)

wins (_, p, b) = tdmg p >= b

superPlan :: [Plan] -> [Player] -> [Plan]
superPlan [] _ = []
superPlan (p:ps) (pl:pls) = filter validPlan $ newPlans p pl ++ superPlan ps pls

fromPlans [] w = w
fromPlans [[]] w = w
fromPlans plans ws =
  let ends_plans = map (\p -> (simPlan p, p)) plans
      ends_winners = filter (\(f,_) -> wins f) ends_plans
      players = map ((\(_,pl,_) -> pl) . fst) ends_winners
      winners = map snd ends_winners
  in
    trace (show $ length plans) $
    if (not.null) winners
      then trace (show winners) $ fromPlans (superPlan plans players) $ take 1 $ sortCost winners
    else fromPlans (superPlan plans players) ws

main = do
  print "Day 22!"
  print player
  print $ getPwnd player
  print $ applyEffects 1 (player {fx = [getFx "poi"]})
  print $ player `cast` poi
  print $ player `cast` rec
  print $ player `cast` shi `hasFx` shi
  -- print $ doTurn (1, player, bossHp) poi
  -- print $ simPlan [poi, rec, shi]
  -- print $ take 2 $ iterate superPlan [[poi,rec]]
  -- print $ validPlan [shi,shi,shi,shi,shi,shi]
  -- print $ fromPlans [[poi, rec, shi]] []
  -- [[poi,rec,shi,poi,rec,poi,dra,rec,mag]]
  -- print $ total cst [poi,rec,shi,poi,rec,poi,dra,rec,mag]
  --   -- 1445
  -- print $ simPlan [poi,rec,shi,poi,rec,poi,dra,rec,mag]
  -- let whats = [[poi,rec,shi,poi,rec,poi,dra,rec,mag],[poi,rec,shi,poi,rec,poi,dra,rec,dra],[poi,rec,shi,poi,rec,poi,shi,rec,mag],[poi,rec,shi,poi,rec,poi,rec,dra,mag],[poi,rec,shi,poi,rec,poi,rec,dra,dra],[poi,rec,shi,poi,rec,poi,rec,shi,mag],[poi,rec,shi,rec,poi,poi,rec,dra,mag],[poi,rec,shi,rec,poi,poi,rec,dra,dra],[poi,rec,shi,rec,poi,poi,rec,shi,mag]]
  -- print $ map (total cst) whats
  -- print $ total cst [poi,rec,shi,poi,rec,poi,poi,mag]

  {-
    Alright...what the heck haha.
    ..... Lots of lots of calcs .....

      2 s 2 r -> 92 hp, 776 + 276 = 1052 mp
      3 p 1 m 2 s 2 r = 1256, not the answer
      3 p 1 m 1d 1 s 2 r = 1216
      omg i read it as 75 not 73
      3 p 1 m 1 d 1s 1r = 987

      3p 2m 1s 1r doesnt work
      3p 1m 1d 1s 1r doesnt work.
      2r 2s 3p 1m works - but 1256 doesnt work.
      I can take 8 hits, so I have 9 actions
      2r 1s 3p 2m 1d  -> 1269
      ...... the right answer.
      Wait... but 1256 doesn't work?!
      > YOU MUST SELECT ONE OF YOUR spells
      > MUST
      > ._.

  -}
  print $ simPlan [poi, rec, shi, poi, rec, dra, poi, mag, mag]
  let badPlayer = player {fx = [("hm", hardModeTick, 999999)]}
      newPlan = [poi, rec, shi, poi, rec, shi, poi, mag, mag ]
  print $ simPlanP newPlan badPlayer
    -- We'll need another shield cast
    -- 56 damage on my turn; kills boss on his next turn.
