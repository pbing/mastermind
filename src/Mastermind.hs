module Mastermind where

import Data.List

type Peg = Int

type Answer = (Int, Int) -- (number of correct positions, number of correct colors)

type State = ([Peg], Answer)

colorRange :: (Peg, Peg)
colorRange = (1, 6)

allColors :: [Peg]
allColors = [fst colorRange .. snd colorRange]

allPossibilities :: [[Peg]]
allPossibilities = [[a, b, c, d] | a <- allColors, b <- allColors, c <- allColors, d <- allColors]

legalPossibilities :: State -> [[Peg]] -> [[Peg]]
legalPossibilities state = checkResult . filter legal
  where
    legal' :: State -> [Peg] -> Bool
    legal' (guess, ans) x = answers guess x == ans
    legal :: [Peg] -> Bool
    legal = legal' state

checkResult :: [[Peg]] -> [[Peg]]
checkResult xs =
  if null xs
    then error "Should not happen"
    else xs

-- https://mathworld.wolfram.com/Mastermind.html
answers :: [Peg] -> [Peg] -> Answer
answers guess code = (b, w)
  where
    b = count id (zipWith (==) guess code)
    w = sum (zipWith min ci gi) - b
    ci = countColors code
    gi = countColors guess
    countColors xs = map ((\fn -> fn xs) . count . (==)) allColors

count :: (a -> Bool) -> [a] -> Int
count p = foldr (\x -> if p x then (1+) else id) 0
