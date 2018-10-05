module Main where

import Lib

main :: IO ()
main = do
  print "What is your name?"
  name <- getLine
  print ("Hello " ++ name ++ "!")
