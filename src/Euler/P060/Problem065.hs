module Euler.P060.Problem065
  ( prob065
  )
  where

{-
 - The square root of 2 can be written as an infinite continued fraction.
 -
 - √2 = 1 +
 - 1
 -   2 +
 - 1
 -     2 +
 - 1
 -       2 +
 - 1
 -         2 + ...
 - The infinite continued fraction can be written, √2 = [1;(2)], (2) indicates that 2 repeats ad infinitum. In a similar way, √23 = [4;(1,3,1,8)].
 -
 - It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations. Let us consider the convergents for √2.
 -
 - 1 +
 - 1
 - = 3/2
 -
 - 2
 -
 - 1 +
 - 1
 - = 7/5
 -   2 +
 - 1
 -
 - 2
 -
 - 1 +
 - 1
 - = 17/12
 -   2 +
 - 1
 -
 -     2 +
 - 1
 -
 -
 - 2
 -
 - 1 +
 - 1
 - = 41/29
 -   2 +
 - 1
 -     2 +
 - 1
 -
 -       2 +
 - 1
 -
 -
 - 2
 -
 - Hence the sequence of the first ten convergents for √2 are:
 -
 - 1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
 - What is most surprising is that the important mathematical constant,
 - e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
 -
 - The first ten terms in the sequence of convergents for e are:
 -
 - 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
 - The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
 -
 - Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
 -}

import Data.Ratio  ((%), numerator, Rational)
import Data.Digits (digits)
import Data.List   (foldr1, inits, intercalate)

prob065 :: Integer
prob065 = prob065' 100

-- naive method

prob065' :: Integer -> Integer
prob065' (fromIntegral->limit) = sum . digits 10 . numerator . (!! (limit - 1))
                               . fmap (sumConvergents 2) . inits $ exs
  where
    exs = [1] ++ intercalate [1,1] (fmap pure [2,4..])

sumConvergents :: Integer -> [Integer] -> Rational
sumConvergents n [] = (n % 1)
sumConvergents n xs = (n % 1) + (1 / foldr1 (\x acc -> x + (1 / acc)) (fmap (% 1) xs))
