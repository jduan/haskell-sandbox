import Data.Char (isPunctuation, isSpace)
import Data.Text as T
import Palindrome
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_reverseInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_whitespaceInvariant
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
