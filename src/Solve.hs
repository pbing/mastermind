module Solve where

import Mastermind

-- Brute force, ignore the answers and go through all possible states.
-- This takes 648.5 iterations on average.
solve1 :: [Peg] -> [Peg] -> ([Peg], Int)
solve1 _ secret = solve1' (head allPossibilities) (tail allPossibilities) 1
  where
    solve1' :: [Peg] -> [[Peg]] -> Int -> ([Peg], Int)
    solve1' g legal acc
      | solved ans = (g, acc)
      | otherwise = solve1' g' legal' (acc + 1)
      where
        ans = answers g secret
        g' = head legal
        legal' = tail legal

solve2 :: [Peg] -> [Peg] -> ([Peg], Int)
solve2 guess secret = solve2' guess allPossibilities 1
  where
    solve2' :: [Peg] -> [[Peg]] -> Int -> ([Peg], Int)
    solve2' g legal acc
      | solved ans = (g, acc)
      | otherwise = solve2' g' legal' (acc + 1)
      where
        ans = answers g secret
        g' = head legal'
        legal' = legalPossibilities (g, ans) legal

solved :: Answer -> Bool
solved = (==) (4, 0)
