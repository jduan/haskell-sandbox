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

-- Use Either type to get better error messages
isPrime' :: Int -> Either String Bool
isPrime' n
  | n < 2 = Left "Numbers less than 2 are not candidates for primes"
  | n >= length primes = Left "Value exceeds limits of prime checker"
  | otherwise = Right (n `elem` primes)

data PrimeError
  = TooLarge
  | InvalidValue
  deriving (Eq)

instance Show PrimeError where
  show TooLarge = "Value exceeds max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

-- The great thing about Either is that because the Left constructor can be
-- any type, there’s no limit to how expressive you can be. If you wanted
-- to, you could return a function!
isPrime'' :: Int -> Either PrimeError Bool
isPrime'' n
  | n < 2 = Left InvalidValue
  | n >= length primes = Left TooLarge
  | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's a prime"
displayResult (Right False) = "It's not a prime"
displayResult (Left primeError) = show primeError
