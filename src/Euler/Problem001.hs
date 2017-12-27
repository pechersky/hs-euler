module Euler.Problem001
  ( prob001, multiple
  )
  where


{- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
 - The sum of these multiples is 23.
 -
 - Find the sum of all the multiples of 3 or 5 below 1000. -}

multiple :: Integral a => a -> a -> Bool
multiple large small = (large `mod` small) == 0

prob001' :: Integer -> Integer
prob001' bound = sum [x | x <- [0..(bound-1)], multiple x 3 || multiple x 5]

prob001 :: Integer
prob001 = prob001' 1000
