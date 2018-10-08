-- The only function you want to export is isPalindrome
module Palindrome
  ( isPalindrome
  ) where

-- Selectively import only 3 functions!
import Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Text as T

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

toLowerCase :: T.Text -> T.Text
toLowerCase = T.map toLower

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preprocess text
