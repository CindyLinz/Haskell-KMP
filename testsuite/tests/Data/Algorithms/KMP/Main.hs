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

import Control.Monad
  ( foldM
  )

check word text =
  let
    table = build word
    res1 = match table text
    res2 = findIndices (isPrefixOf word) (tails text)
  in take 100 res1 == take 100 res2

dataset =
  [ ("abababcaba", "abababababcabababcababbb")
  , ("ababcabababc", "ababcabababcabababc")
  , ("ababcabababc", cycle "ababcabababcabababc")
  , ("", "abca")
  , ("abca", "")
  ]

main = do
  res <- foldM
    ( \res (word, text) -> do
      if check word text
        then return res
        else do
          putStrLn "Failed dataset:"
          putStrLn $ "  " ++ word
          putStrLn $ "  " ++ take 100 text ++ if null (drop 100 text) then "" else "..."
          return False
    )
    True
    dataset

  if res
    then exitSuccess
    else exitFailure
