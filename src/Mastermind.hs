module Mastermind where

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
legalPossibilities (guess, ans) = filter legal
  where
    legal :: [Peg] -> Bool
    legal xs = answers guess xs == ans

-- https://mathworld.wolfram.com/Mastermind.html
answers :: [Peg] -> [Peg] -> Answer
answers guess code = (b, w)
  where
    b = count id (zipWith (==) guess code)
    w = sum (zipWith min ci gi) - b
    ci = countColors code
    gi = countColors guess
    countColors xs = map ((\fn -> fn xs) . count . (==)) allColors

-- return number of list elements when predicate is true
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
