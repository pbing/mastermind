module Solve where

import Mastermind

-- Brute force, ignore the answers and go through all possible states.
-- This takes 648.5 iterations on average.
solve1 :: [Peg] -> [Peg] -> ([Peg], Int)
solve1 _ secret = solve1' (head allPossibilities) secret (tail allPossibilities) 1
  where
    solve1' :: [Peg] -> [Peg] -> [[Peg]] -> Int -> ([Peg], Int)
    solve1' guess secret legal acc
      | solved (answers guess secret) = (guess, acc)
      | otherwise = solve1' (head legal) secret (tail legal) (acc + 1)

solve2 :: [Peg] -> [Peg] -> ([Peg], Int)
solve2 guess secret = solve2' guess secret allPossibilities 1
  where
    solve2' :: [Peg] -> [Peg] -> [[Peg]] -> Int -> ([Peg], Int)
    solve2' guess secret legal acc
      | solved (answers guess secret) = (guess, acc)
      | otherwise = solve2' guess' secret legal' (acc + 1)
      where
        ans = answers guess secret
        legal' = legalPossibilities (guess, ans) legal
        guess' = head legal'

solved :: Answer -> Bool
solved = (==) (4, 0)
