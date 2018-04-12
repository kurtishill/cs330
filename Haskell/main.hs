import Data.Array

--Part 1

iSqrt :: Int -> Int
iSqrt n = floor(sqrt(fromIntegral n))

isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..(iSqrt n)], n `mod` x == 0]

primes :: [Int]
primes = filter isPrime [2..]

isPrimeFast :: Int -> Bool
isPrimeFast n = null [x | x <- (takeWhile (<=(iSqrt n)) primesFast), n `mod` x == 0]

primesFast :: [Int]
primesFast = 2:filter isPrimeFast [3..]

--Part 2

lcsLength :: String -> String -> Int
lcsLength s1 s2 = a ! (0,0) where
  n = length s1 -- get length of string #1
  m = length s2 -- get length of string #2
  a = array ((0,0), (n,m))
    ([((i,m), 0) | i <- [0..n]] ++
    ([((n,j), 0) | j <- [0..m]] ++
    ([((i,j), func char1 char2 i j) | (char1, i) <- zip s1 [0..], (char2, j) <- zip s2 [0..]])))

  func char1 char2 i j
    | char1 == char2 = 1 + (a!(i + 1, j + 1))
    | otherwise = max (a!(i, j + 1)) (a!(i + 1, j))
