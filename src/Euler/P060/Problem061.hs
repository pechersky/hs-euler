module Euler.P060.Problem061
  ( prob061
  )
  where

{-
 - Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are
 - all figurate (polygonal) numbers and are generated by the following formulae:
 -
 - Triangle    P3,n=n(n+1)/2   1, 3, 6, 10, 15, ...
 - Square      P4,n=n2         1, 4, 9, 16, 25, ...
 - Pentagonal  P5,n=n(3n−1)/2  1, 5, 12, 22, 35, ...
 - Hexagonal   P6,n=n(2n−1)    1, 6, 15, 28, 45, ...
 - Heptagonal  P7,n=n(5n−3)/2  1, 7, 18, 34, 55, ...
 - Octagonal   P8,n=n(3n−2)    1, 8, 21, 40, 65, ...
 - The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.
 -
 - The set is cyclic, in that the last two digits of each number is the first two
 - digits of the next number (including the last number with the first).
 - Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal
 - (P5,44=2882), is represented by a different number in the set.
 - This is the only set of 4-digit numbers with this property.
 - Find the sum of the only ordered set of six cyclic 4-digit numbers for which
 - each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and
 - octagonal, is represented by a different number in the set.
 -}

import qualified Data.IntSet                 as IS
import           Data.Digits                 (digits)
import           Data.List                   (transpose, iterate)
import           Control.Monad               (guard)
import           Control.Monad.Trans.State   (StateT, evalStateT, get, put)
import           Control.Monad.Trans.Class   (lift)

prob061 :: Integer
prob061 = prob061' 6

-- naive method

prob061' :: Integer -> Integer
prob061' (fromIntegral->limit) = sum . head . filter cyclic . concat
                               . flip evalStateT (IS.fromList [1..limit - 1]) . (!! (limit - 1))
                               . iterate (>>= go) . pure
                               $ pure <$> head valid
  where
    valid = fmap (takeWhile (< 1e4) . dropWhile (< 1e3)) shapes
    go :: [[Integer]] -> StateT IS.IntSet [] [[Integer]]
    go xs = do
      x <- lift xs
      ixs <- get
      ix <- lift $ IS.toList ixs
      let shape = valid !! ix
      y <- lift shape
      guard (match y (head x))
      put $ IS.difference ixs (IS.singleton ix)
      pure . fmap (y :) . filter (match y . head) $ xs

triangles :: [Integer]
triangles = scanl1 (+) [1..]

shapes :: [[Integer]]
shapes = transpose . zipWith (\a -> scanl (+) a . repeat) triangles $ 0 : triangles

cyclic :: [Integer] -> Bool
cyclic [] = True
cyclic [_] = True
cyclic xs = and $ zipWith match xs (drop 1 . cycle $ xs)

match :: Integer -> Integer -> Bool
match x y = (reverse . take 2 . reverse . digits 10 $ x) == (take 2 . digits 10 $ y)
