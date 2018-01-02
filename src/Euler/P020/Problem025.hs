module Euler.P020.Problem025
  ( prob025, fib
  )
  where

{-
 -The Fibonacci sequence is defined by the recurrence relation:
 -
 -Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
 -Hence the first 12 terms will be:
 -
 -F1 = 1
 -F2 = 1
 -F3 = 2
 -F4 = 3
 -F5 = 5
 -F6 = 8
 -F7 = 13
 -F8 = 21
 -F9 = 34
 -F10 = 55
 -F11 = 89
 -F12 = 144
 -The 12th term, F12, is the first term to contain three digits.
 -
 -What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
 -}

prob025 :: Integer
prob025 = prob025' 1000

-- naive method

prob025' :: Integer -> Integer
prob025' (fromIntegral->limit) = fst . head . dropWhile ((< limit) . length . show . snd)
                               . zip [1..] . fmap fib $ [1..]

fix :: (a -> a) -> a
fix f = let x = f x in x

memoize :: (Int -> a) -> (Int -> a)
memoize f = (fmap f [0..] !!)

fib :: Int -> Integer
fib = fix (memoize . go)
  where
    go :: (Int -> Integer) -> Int -> Integer
    go f n
      | n == 1 = 1
      | n == 2 = 1
      | otherwise = f (n - 1) + f (n - 2)
