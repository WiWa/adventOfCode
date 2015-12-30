import Data.List
import Data.Function (on)
-- Brute Force, starting with Rec Poi Shield.
-- (Cost, Damage, MP, HP)
type Turn = Int
type BossHp = Int
type Fight = (Turn, Player, BossHP)

data Player = Player{
    hp    :: Int
  , mp    :: Int
  , tcst  :: Int
  , tdmg  :: Int
  , fx    :: [Effect]
} deriving (Show)
type Effect = (Spell, Int)
data Spell = Spell{
    name  :: String
  , cst   :: Int
  , dmg   :: Int
  , dhp    :: Int
  , dmp    :: Int
}
instance Show Spell where
  show = name

type Plan = [Spell]

bossHp = 58
bossDmg = 9

total :: (Spell -> Int) -> Plan -> Int
total f plan = sum $ map f plan

sortCost plans = sortBy (compare `on` total cst) plans

mag = Spell "mag" 53 4 0 0
dra = Spell "dra" 73 2 2 0
shi = Spell "shi" 113 0 7 0
poi = Spell "poi" 173 3 0 0
rec = Spell "rec" 229 0 0 101

spells = [mag, dra, shi, poi, rec]

minMp = foldl min maxBound $ map dmp spells

player = Player 50 500 0 []

newPlans :: Plan -> [Plan]
newPlans plan
  | total dmp plan <= minMp = [plan ++ [rec]]
  | otherwise = map (\s -> plan ++ [s]) spells

applyFx p@Player{hp = hp, mp = mp, tcst = tcst, tdmg = tdmg, fx = fx} fxname t =
  case fxname of
    "poi" -> Player hp mp tcst (tdmg + dmg poi) fx
    "shi" -> Player (hp + dhp shi) mp tcst tdmg fx
    "rec" -> Player hp (mp + dmp rec) tcst tdmg fx


applyEffects p@Player{hp = hp, mp = mp, tcst = tcst} t =
  let fxs = map (show.fst) $ fx p
  in foldl applyFx p fxs t

doTurn fight@(turn, p, bhp) =
  let xs@(newp, pdmg) = applyEffects p t
  in  xs

main = do
  print "Day 22!"
  print $ player
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

    13  38  34  Drain  75   379-304
    15  31  26  MagMis 53   405-330
    17  22  16  Poison 173  374-201
    19  13  10  MagMis 53
    21  4   0
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
      omfg i read it as 75 not 73
      3 p 1 m 1 d 1s 1r = 987
  -}
