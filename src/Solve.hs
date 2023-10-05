module Solve where

import Mastermind

-- Brute force, ignore the answers and go through all possible states.
-- This takes maximum 1296 iterations.
solve1 :: [Peg] -> [Peg] -> ([Peg], Int)
solve1 _ secret = go allPossibilities 1
  where
    go (x:xs) n
      | isSolved (answers x secret) = (x, n)
      | otherwise = go xs (n + 1)
    go [] _ = error "Should not happen"

-- Calculate new possible states from answer.
-- This takes maximum 9 iterations.
solve2 :: [Peg] -> [Peg] -> ([Peg], Int)
solve2 guess secret = go guess allPossibilities 1
  where
    go g xs n
      | isSolved ans = (g, n)
      | otherwise = go g' xs' (n + 1)
      where
        ans = answers g secret
        g' = head xs'
        xs' = legalPossibilities (g, ans) xs
