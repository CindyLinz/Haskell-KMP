-- |This module implements the Knuth-Morris-Pratt algorithm.
-- It can search a word in a text in O(m+n) time, where m and n are the length of the word and the text.
--
-- This module can apply on any list of instance of Eq.
--
-- Donald Knuth; James H. Morris, Jr, Vaughan Pratt (1977).
-- Fast pattern matching in strings.
-- SIAM Journal on Computing 6 (2): 323-350. doi:10.1137/0206024
--
-- Sample usage:
--
-- @
--  let
--    word = \"abababcaba\"
--    text = \"abababababcabababcababbb\"
--    kmpTable = build word
--    result = match kmpTable text
--    -- the result should be [4, 11]
-- @
--
module Data.Algorithms.KMP
  ( Table
  , build
  , match
  ) where

import Data.Array
  ( Array
  , listArray
  , bounds
  , (!)
  )

-- |The solid data type of KMP table
data Table a = Table
  { alphabetTable :: Array Int a
  , jumpTable :: Array Int Int
  }

-- |The 'build' function eats a pattern (list of some Eq) and generates a KMP table.
--
-- The time and space complexities are both O(length of the pattern)
build :: Eq a => [a] -> Table a
build pattern =
  let
    len = length pattern

    resTable = Table
      { alphabetTable = listArray (0,len-1) pattern
      , jumpTable = listArray (-1,len-1) $ (-2) : genJump (-1) 0
      }

    genJump _ 0 =
      let
        o = if 1 == len || alphabetTable resTable ! 0 /= alphabetTable resTable ! 1
          then -1
          else -2

        later = genJump (-1) 1
      in
        o : later

    genJump lastMPJump i =
      let
        ch = alphabetTable resTable ! i

        findJ j
          | j == -2 = -2
          | alphabetTable resTable ! (j+1) == ch = j
          | j == -1 = -2
          | otherwise = findJ (jumpTable resTable ! j)

        j = findJ lastMPJump

        o = if i+1 == len || alphabetTable resTable ! (i+1) /= alphabetTable resTable ! (j+2)
          then j+1
          else jumpTable resTable ! (j+1)

        later = genJump (j+1) (i+1)
      in o : later

  in
    resTable

-- |The 'match' function takes the KMP table and a list to be searched (might be infinite)
-- and then generates the search results as a list of every matched begining (might be infinite).
--
-- The time complexity is O(length of the pattern + length of the searched list)
match :: Eq a => Table a -> [a] -> [Int]
match table str =
  let
    len = 1 + snd ( bounds (alphabetTable table) )

    go i j str =
      let
        later = case str of
          (s:ss) ->
            let
              (i', j', str')
                | j < 0 || j < len && s == alphabetTable table ! j = (i + 1, j + 1, ss)
                | otherwise = (i, 1 + (jumpTable table ! (j - 1)), str)
            in
              go i' j' str'
          _ -> []
      in
        if j == len
          then i-len : later
          else later
  in
    go 0 0 str
