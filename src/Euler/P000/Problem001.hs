module Euler.P000.Problem001
  ( prob001
  )
  where


{- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
 - The sum of these multiples is 23.
 -
 - Find the sum of all the multiples of 3 or 5 below 1000. -}

prob001 :: Integer
prob001 = prob001'' 1000

-- naive method

multiple :: Integral a => a -> a -> Bool
multiple large small = (large `mod` small) == 0

prob001' :: Integer -> Integer
prob001' bound = sum [x | x <- [0..(bound-1)], multiple x 3 || multiple x 5]


-- faster calculations

sumMultiplesUpTo' :: Integral a => a -> a -> [a]
sumMultiplesUpTo' jump bound = [0,jump..bound]

sumMultiplesUpTo :: Integral a => a -> a -> a
sumMultiplesUpTo jump bound = jump * sumTo (bound `div` jump)

sumTo' :: Integral a => a -> a
sumTo' bound = sum [0..bound]

sumTo :: Integral a => a -> a
sumTo bound = (bound * (bound + 1)) `div` 2

prob001'' :: Integer -> Integer
prob001'' bound = sumMultiplesUpTo 3 (bound - 1) + sumMultiplesUpTo 5 (bound - 1) - sumMultiplesUpTo (3 * 5) (bound - 1)
