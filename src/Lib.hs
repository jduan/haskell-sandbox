module Lib
  ( someFunc
  ) where

-- Use "Lib.length" to reference the length defined in this module.
-- Without "Lib.", GHC will complaint about Prelude.length
someFunc :: IO ()
someFunc = putStrLn $ "someFunc" ++ show Lib.length

length :: Int
length = 8

head :: Monoid a => [a] -> a
head (x:xs) = x
head [] = mempty
