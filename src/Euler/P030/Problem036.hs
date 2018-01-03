module Euler.P030.Problem036
  ( prob036
  )
  where

{- The decimal number, 585 = 1001001001_2 (binary), is palindromic in both bases.
 -
 - Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
 -
 - (Please note that the palindromic number, in either base, may not include leading zeros.) -}

import Data.Digits (digits)

prob036 :: Integer
prob036 = prob036' 1e6

-- naive method

prob036' :: Integer -> Integer
prob036' limit = sum . filter pal . enumFromTo 1 $ limit
  where
    pal n = all palindrome [digits 10 n, digits 2 n]
    palindrome xs = and $ zipWith (==) xs (reverse xs)
