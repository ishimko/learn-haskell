fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

primes :: [Integer]
primes = sieve [2..] where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]