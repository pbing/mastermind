module Solve where

import Mastermind

-- Brute force, ignore the answers and go through all possible states.
-- This takes 648.5 iterations on average.
solve1 :: [Peg] -> [Peg] -> ([Peg], Int)
solve1 _ secret = solve1' (head allPossibilities) secret (tail allPossibilities) 1
  where
    solve1' :: [Peg] -> [Peg] -> [[Peg]] -> Int -> ([Peg], Int)
    solve1' g s legal acc
      | solved (answers g s) = (g, acc)
      | otherwise = solve1' (head legal) s (tail legal) (acc + 1)

solve2 :: [Peg] -> [Peg] -> ([Peg], Int)
solve2 guess secret = solve2' guess secret allPossibilities 1
  where
    solve2' :: [Peg] -> [Peg] -> [[Peg]] -> Int -> ([Peg], Int)
    solve2' g s legal acc
      | solved (answers g s) = (g, acc)
      | otherwise = solve2' g' s legal' (acc + 1)
      where
        ans = answers g s
        legal' = legalPossibilities (g, ans) legal
        g' = head legal'

solved :: Answer -> Bool
solved = (==) (4, 0)
