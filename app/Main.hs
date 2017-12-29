{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.Printf (printf)
import Language.Haskell.TH

import Euler()

main :: IO ()
main = print "helloworld"

prob :: Integer -> Q Exp
prob num = varE (mkName ("prob" ++ printf "%03d" num))

