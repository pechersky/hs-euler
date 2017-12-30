module Euler.P010.Problem015
  ( prob015
  )
  where

{- Starting in the top left corner of a 2×2 grid,
 - and only being able to move to the right and down,
 - there are exactly 6 routes to the bottom right corner.
 -
 -
 - How many such routes are there through a 20×20 grid? -}

prob015 :: Integer
prob015 = prob015' 20

-- naive method

prob015' :: Integer -> Integer
prob015' (fromIntegral->pos) = travel pos pos

travel :: Int -> Int -> Integer
travel x y = iterate (scanl (+) 1 . tail) (repeat 1) !! y !! x
