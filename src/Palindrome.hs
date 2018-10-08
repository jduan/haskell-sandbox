-- The only function you want to export is isPalindrome
module Palindrome
  ( isPalindrome
  ) where

-- Selectively import only 3 functions!
import Data.Char (isPunctuation, isSpace, toLower)

stripWhiteSpace :: String -> String
stripWhiteSpace = filter (not . isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

toLowerCase :: String -> String
toLowerCase = map toLower

preprocess :: String -> String
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where
    cleanText = preprocess text
