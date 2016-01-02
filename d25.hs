firstCode = 20151125
nextFactor = 252533
nextDivRem = 33554393

-- pos = (2981, 3075)
pos = (3075, 2981)

nextCode code = rem (nextFactor * code) nextDivRem

getTrinumFrom _ 0 = 0
getTrinumFrom x y = x + getTrinumFrom (x + 1) (y - 1)

getTrinum = getTrinumFrom 1

codeNum (x, y) = getTrinum x + getTrinumFrom x (y - 1)

codeAtNum 1 = firstCode
codeAtNum n = nextCode $ codeAtNum $ n - 1

codeAt = codeAtNum . codeNum

main = do
  print "Day 25! Merry Chri-... new years..."
  print $ codeAt pos
