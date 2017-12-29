module Euler.P000.Problem005
  ( prob005
  )
  where

import Euler.Factors (runFactors)

{- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 -
 - What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20? -}

prob005 :: Integer
prob005 = prob005' 20

-- naive method

prob005' :: Int -> Integer
prob005' bound = toInteger . runFactors . foldr (+) 1 . fmap (fromIntegral . fromEnum) $ [1..bound]
