doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

mario = "It's a-me, Mario"

lostNumbers = [4,8,15,16,23,42]
otherNumbers = [1,2,3,4]
allNumbers = lostNumbers ++ otherNumbers
