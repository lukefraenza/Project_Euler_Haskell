{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

num = 600851475143

--sol3 :: (Integral a) => a
--sol3 = head (filter isPrime (filter isFactor [num-1, num-2 ..]) )

--isFactor :: (Integral a) => a -> Bool
isFactor x = num `mod` x == 0


notMult2s 2 = True
notMult2s x = (x `mod` 2 == 0) == False

notMult3s 3 = True
notMult3s x = (x `mod` 3 == 0) == False

notMult5s 5 = True
notMult5s x = (x `mod` 5 == 0) == False

notMult7s 7 = True
notMult7s x = (x `mod` 7 == 0) == False

notMult11s 11 = True
notMult11s x = (x `mod` 11 == 0) == False

notMult13s 13 = True
notMult13s x = (x `mod` 13 == 0) == False

notMult17s 17 = True
notMult17s x = (x `mod` 17 == 0) == False

notMults x num = (num `mod` x == 0) == False

primes x = filter notMult17s (filter notMult13s (filter notMult11s (filter notMult7s (filter notMult5s (filter notMult3s (filter notMult2s [x-1,x-2..]))))))
primes' x = head ( primes x)

myPrimes x = filter (notMults 3) (filter (notMults 2) [x-1,x-2..])
