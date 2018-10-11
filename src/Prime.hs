module Prime where

-- There are lots of prime numbers. Let's set up a reasonable upperbound.
-- Otherwise, you may consume all the memory of your computer.
primes :: [Int]
primes = sieve [2 .. 10000]

-- Sieve of Eratosthenes:
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
--
-- This approach doesn't work well because the next line won't work!
-- take 5 $ primes
-- sieve nums = reverse $ go nums []
--   where
--     go [] ys = ys
--     go (x:xs) ys = go (filter (isNotMultiples x) xs) (x : ys)
--     isNotMultiples x y = (y `mod` x) /= 0
sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where
    noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

-- There are 3 cases:
-- negative numbers: return Nothing
-- numbers that are too big: return Nothing
-- otherwise: return "Just Bool"
isPrime :: Int -> Maybe Bool
isPrime n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (n `elem` primes)

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just $ go n []
  where
    go 1 factors = reverse factors
    go n factors =
      let factor = firstFactor n
       in go (n `div` factor) (factor : factors)

-- Given a number that's greater than 1, find the first prime factor.
firstFactor :: Int -> Int
firstFactor n = head $ filter ((== 0) . (n `mod`)) primes
