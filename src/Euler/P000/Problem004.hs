module Euler.P000.Problem004
  ( prob004
  )
  where

{- A palindromic number reads the same both ways.
 - The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
 -
 - Find the largest palindrome made from the product of two 3-digit numbers. -}

prob004 :: Integer
prob004 = prob004' $ 1e3 - 1

-- naive method

palindrome :: Integer -> Bool
palindrome = palindromeStr . show
  where
    palindromeStr :: String -> Bool
    palindromeStr str = and $ zipWith (==) str (reverse str)

prob004' :: Integer -> Integer
prob004' bound = maximum . filter palindrome $ [x * y | x <- possible, y <- possible, x <= y]
  where
    possible = [bound, bound-1 .. 1]
