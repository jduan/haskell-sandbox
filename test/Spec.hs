import Data.Char (isPunctuation, isSpace)
import Data.Maybe
import qualified Data.Text as T
import Palindrome
import Prime
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_reverseInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_whitespaceInvariant
  quickCheck prop_validPrimesOnly
  quickCheck prop_primesArePrime
  quickCheck prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  putStrLn "done"

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome reversedText
  where
    reversedText = T.reverse text

prop_whitespaceInvariant text =
  isPalindrome text == isPalindrome noWhitespaceTest
  where
    noWhitespaceTest = T.filter (not . isSpace) text

prop_validPrimesOnly val =
  if val < 2 || val >= length primes
    then isNothing result
    else isJust result
  where
    result = isPrime val

prop_primesArePrime val =
  if result == Just True
    then length divisors == 0
    else True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val =
  if result == Just False
    then length divisors > 0
    else True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal val =
  if result == Nothing
    then True
    else product (fromJust result) == val
  where
    result = primeFactors val
