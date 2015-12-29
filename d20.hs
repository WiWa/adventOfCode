{-# LANGUAGE FlexibleContexts #-}

leastPresents = 33100000

-- http://stackoverflow.com/questions/2807686/
--   whats-the-way-to-determine-if-an-int-is-a-perfect-square-in-haskell
isSq n = sq * sq == n
    where sq = floor $ sqrt (fromIntegral n::Double)

sumDivs :: Integer -> Integer
sumDivs = sumDivs2 (\_ _ -> True)

sumDivs2 :: (Integer -> Integer -> Bool) -> Integer -> Integer
sumDivs2 f n =
  let sqrtn = truncate $ sqrt (fromIntegral n :: Double)
      prehalf = takeWhile (<= sqrtn) [1..]
      half = filter ((== 0).mod n) prehalf
      halfnhalf = sum $ filter (f n) half
      perhaps x = if f n x then x else 0
      done = foldl (\acc x -> acc + perhaps (quot n x)) halfnhalf half
  in  if isSq n then done -  sqrtn else done

numPres :: Integer -> (Integer, Integer)
numPres n = (10 * sumDivs n, n)

numPres2 :: (Integer -> Integer -> Bool) -> Integer -> (Integer, Integer)
numPres2 f n = (11 * sumDivs2 f n, n)

upTo50 n e = 50 * e >= n

byPresents (s,_) = s >= leastPresents

main = do
  print "Day 20!"
  print $ "Finding a house with " ++ show leastPresents
  print $ map numPres [1..9]
  let (pres, house) = head $ filter byPresents $ map numPres [500000..]
  print (pres, house)
    -- 776160
  let (pres2, house2) = head $ filter byPresents $ map (numPres2 upTo50) [776160..]
  print (pres2, house2)
    -- 786240
