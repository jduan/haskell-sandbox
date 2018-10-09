import Data.Char (isPunctuation)
import Data.Text as T
import Palindrome
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_reverseInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  putStrLn "done"

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome reversedText
  where
    reversedText = T.reverse text
