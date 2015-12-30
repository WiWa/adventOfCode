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
noop = Spell "noop" 0 0 0 0 []

inPlan :: Spell -> Plan -> Bool
inPlan s p = name s `elem` map name p

getFx :: String -> Effect
getFx s = head $ sfx (getSpell s)

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

applyFx :: Turn -> Player -> String -> Player
applyFx t p@Player{hp = hp, mp = mp, tcst = tcst, tdmg = tdmg, fx = fx} fxname =
  let
      xs = [poiStr, shiStr, recStr]
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
simPlan plan =
  let base = (1, player, bossHp)
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
  print $ total cst [poi,rec,shi,poi,rec,poi,dra,rec,mag]
    -- 1445
  print $ simPlan [poi,rec,shi,poi,rec,poi,dra,rec,mag]
  let whats = [[poi,rec,shi,poi,rec,poi,dra,rec,mag],[poi,rec,shi,poi,rec,poi,dra,rec,dra],[poi,rec,shi,poi,rec,poi,shi,rec,mag],[poi,rec,shi,poi,rec,poi,rec,dra,mag],[poi,rec,shi,poi,rec,poi,rec,dra,dra],[poi,rec,shi,poi,rec,poi,rec,shi,mag],[poi,rec,shi,rec,poi,poi,rec,dra,mag],[poi,rec,shi,rec,poi,poi,rec,dra,dra],[poi,rec,shi,rec,poi,poi,rec,shi,mag]]
  print $ map (total cst) whats
  print $ total cst [poi,rec,shi,poi,rec,poi,poi,mag]
  {-
    Alright...what the heck haha.
    1   50  58  Shield 113  500-387
    3   48  58  Poison 173  387-214 - Lost

    1   50  58  Shield 113  500-387
    3   48  58  Rechar 229  387-158
    5   46  58  Poison 173  360-187
    7   44  52  Shield 113  389-276
    9   42  46  Rechar 229  377-148
    11  40  40  Poison 173  350-177
    13  38  34  Shield 113  379-266
    15  36  28  Rechar 229  367-138 <- unnecessary
    17  34  22  Poison 173  340-167
    19  32  16  MagMis 53
    21  21  6
    23  12  0
      -- Total 1598, too high!!
    S + P combo is 286 mana, over 6 turns it deals 18 damage and absorbs 21.
    I need to cast R at least once, maybe twice. Gives me 505 - 229 = 276
    If I cast Rechar twice, I get 500 + 500 + 52 = 1052 MP.
    Okay, lets try 1598 - 229 = 1369
      -- Too high!!
    Instead Casting shield thrice, I can try to end faster.
    Poison does 6 dmg every two turns, but I can use drain to do 8.
    Boss does 9 damage every two turns but drain makes it 7. Starting at 40/40
    I can use MagMis/Drain.

    1   50  58  Shield 113  500-387
    3   48  58  Rechar 229  387-158
    5   46  58  Poison 173  360-187
    7   44  52  Shield 113  389-276
    9   42  46  Rechar 229  377-148
    11  40  40  Poison 173  350-177

    13  38  34  Drain  73   379-304
    15  31  26  MagMis 53   405-330
    17  22  16  Poison 173  374-201
    19  13  10  MagMis 53
    21  4   0
      -- 1382
      -- 1362, 2 rec, 2 shi, 3 poi, 3 mag

    1   50  58  poi   173   500-327
    3   41  52  rec   229   327-98
    5   32  46  shi   113   300-187
    7   30  40  poi   173   389-216
    9   28  34  rec   229   317-88

    1   50  58  poi   173   500-327
    3   41  52  rec   229   327-98
    5   32  46  shi   113   300-187
    7   30  40  poi   173   389-216
    9   28  34  mag   53    317-264
    11  26  24  mag   75    264-189
    13  19  14  poi   173
    15  10  10  mag
    17  1   4

    11  26  28  shi   113   290-177
    13  24  22  poi   173   379-206
    15  22  16  mag   53    307-254
    17  20  6
    19  11  0

    11  26  28  dra   75    290-215
    13  21  20  mag   53    417-364
    15  12  10
    17  3   6
    18  0

      If i cast 3 poisons, I need to last 18 turns, 9 of which are hits.
      So i need 81 HP. (really 82).
      Shield gives me 21 hp for 113 MP
      I need 3 poisons + 4 more damage for 58 damage.
      3 Poisons = 3 * 173 = 519, + shield = 632. 2 magmis is 106, 738 MP.
      Add in recharge and I get to 967 MP. rec + shield = 334
      Okay well if I end it 4 turns earlier I only need

      What about 2 poison (36 dmg), 2 Shield (92 HP) 1 Rech = 801 MP
      Then 2P, 2S, 3M = 36 + 12 = 48 damage, not enough.
      The only way to get enough dmg from 1 R is 3P + 1M = 58.
      But this leaves us with only 1S = 71 HP = 2 * 7 + 1 Turns
      If we use 1M + 1D, we get 2 * 8 + 1 Turns because we have 73 HP.
      this does 3 * (2 * 8 + 1) = 51 + 4 + 2 +
      2 1 1 5
      17 turns
      13th turn poison deals 36 damage
      i took 7 actions, 3 p, 1s, 1 r and 2m
      17th turn poison dealt 12 damage.
      18th turn poison dealt 15.
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

  -}
