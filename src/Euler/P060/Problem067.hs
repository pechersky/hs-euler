module Euler.P060.Problem067
  ( prob067
  )
  where

{-
 - By starting at the top of the triangle below and moving to adjacent numbers
 - on the row below, the maximum total from top to bottom is 23.
 -
 - 3
 - 7 4
 - 2 4 6
 - 8 5 9 3
 -
 - That is, 3 + 7 + 4 + 9 = 23.
 -
 - Find the maximum total from top to bottom in triangle.txt
 - (right click and 'Save Link/Target As...'),
 - a 15K text file containing a triangle with one-hundred rows.
 -
 - NOTE: This is a much more difficult version of Problem 18
 - It is not possible to try every route to solve this problem,
 - as there are 2^99 altogether!
 - If you could check one trillion (10^12) routes every second it would take
 - over twenty billion years to check them all.
 - There is an efficient algorithm to solve it. ;o)
 -}

import Data.List (foldr1)

prob067 :: IO Integer
prob067 = do
  input <- readFile txtfile
  return $ prob067' . lines $ input
  where
    txtfile = "src/Euler/P060/p067_triangle.txt"

-- naive method

prob067' :: [String] -> Integer
prob067' = head . foldr1 addLevel . fmap (fmap read . words)

addLevel :: [Integer] -> [Integer] -> [Integer]
addLevel sl bl = init . tail $ zipWith max ((++ [0]) $ add' (0 : sl)) (0 : add' (sl ++ [0]))
  where
    add' = zipWith (+) bl
