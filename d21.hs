
main = do
  print "Day 21!"
  -- Okay part 1 is doable by hand:
  -- if we have 8 dmg 2 amr we win because first turn
  -- similarly if we have 7 dmg and 3 amr
  -- then, we can optimize by hand: 7 dmg, 2 amr, 1 amr ring
  print "91"
  -- part 2 is similar. We need to have under 7/3 or 8/2
  -- "Lots of planning wtf doesnt work"
    -- oh armor is optional and you can get 0-2 rings, oops, reading op
  -- 4-9/5-0
  -- 4/5: 8 + 102, 8 + 31 + 80 = 119
  -- 5/4: 8 + 25 + 80 + 13 = 126
  -- 6/3: 8 + 50 + 80 = 138
  -- 7/2: 8 + 100 + 40 = 148
  -- 8/1: 10 + 100 + 20 = 130
  -- 9/0: 8 + 100 + 50 = 158
  print "158"
  -- brute force: order combinations by desc cost filtering by win/lose condition
