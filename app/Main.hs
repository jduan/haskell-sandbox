module Main where

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HttpExample
import Lib
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Palindrome

-- main :: IO ()
-- main = do
--   TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
--   text <- TIO.getLine
--   let response =
--         if isPalindrome text
--           then "it is!"
--           else "it's not!"
--   TIO.putStrLn response
--
main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatus response
  if statusCode status == 200
    then do
      print "saving request to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else print $ statusMessage status
