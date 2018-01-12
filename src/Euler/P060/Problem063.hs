module Euler.P060.Problem063
  ( prob063
  )
  where

{-
 - The 5-digit number, 16807=75, is also a fifth power.
 - Similarly, the 9-digit number, 134217728=89, is a ninth power.
 -
 - How many n-digit positive integers exist which are also an nth power?
 -}

prob063 :: Integer
prob063 = prob063'

-- naive method

prob063' :: Integer
prob063' = fromIntegral . sum . takeWhile (> 0) . fmap f $ [1..]
  where
    log10 = logBase 10
    p :: (Integer -> Integer -> Bool) -> Double -> Double -> Bool
    p c b x = floor x `c` (floor (x * log10 b) + 1)
    f :: Double -> Int
    f b = length . takeWhile (p (==) b) . takeWhile (p (<=) b) $ [1..]
