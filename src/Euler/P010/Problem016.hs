module Euler.P010.Problem016
  ( prob016
  )
  where

{-
 -2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 -
 -What is the sum of the digits of the number 2^1000?
 -}

prob016 :: Integer
prob016 = prob016' 1000

-- naive method

prob016' :: Integer -> Integer
prob016' = sum . fmap (read . replicate 1) . show @Integer . (2 ^)
