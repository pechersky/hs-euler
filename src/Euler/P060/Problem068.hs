module Euler.P060.Problem068
  ( prob068
  )
  where

{-
 - Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.
 -
 - Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.
 -
 - It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
 -
 - Total Solution Set
 - 9  4,2,3; 5,3,1; 6,1,2
 - 9  4,3,2; 6,2,1; 5,1,3
 - 10 2,3,5; 4,5,1; 6,1,3
 - 10 2,5,3; 6,3,1; 4,1,5
 - 11 1,4,6; 3,6,2; 5,2,4
 - 11 1,6,4; 5,4,2; 3,2,6
 - 12 1,5,6; 2,6,4; 3,4,5
 - 12 1,6,5; 3,5,4; 2,4,6
 - By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.
 -
 - Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings. What is the maximum 16-digit string for a "magic" 5-gon ring?
 -}

import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad (guard)

prob068 :: String
prob068 = prob068' 10

-- naive method

prob068' :: Integer -> String
prob068' limit = maximum . fmap showf . filter f . concatMap go $ [1..(limit*3)]
  where
    go val = flip evalStateT [1..limit] $ do
      a' <- StateT select
      a  <- StateT select
      b  <- StateT select
      guard (p a' a b)
      b' <- StateT select
      c  <- StateT select
      guard (p b' b c)
      c' <- StateT select
      d  <- StateT select
      guard (p c' c d)
      d' <- StateT select
      e  <- StateT select
      guard (p d' d e)
      e' <- StateT select
      guard (p e' e a)
      return [tup a' a b, tup b' b c, tup c' c d, tup d' d e, tup e' e a]
        where
          p x y z = val == x + y + z
    tup x y z = (x, y, z)
    f [] = False
    f (x:xs) = all (> (fst' x)) (fmap fst' xs)
    fst' (x,_,_) = x
    showf = concatMap (\(x,y,z) -> concatMap show [x,y,z])

select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]
