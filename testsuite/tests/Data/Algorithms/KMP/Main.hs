module Main where

import Data.Algorithms.KMP
import Data.List
  ( findIndices
  , isPrefixOf
  , tails
  )

import System.Exit
  ( exitFailure
  , exitSuccess
  )

main = do
  let
    pattern = "abababcaba"
    target = cycle "abababababcabababcababbb"
    table = build pattern
    res1 = take 100 $ match table target
    res2 = take 100 $ findIndices (isPrefixOf pattern) (tails target)

  if res1 == res2
    then exitSuccess
    else exitFailure
