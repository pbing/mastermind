module Main where

import Control.Monad

import Mastermind
import Random
import Solve
import Statistics

main :: IO ()
-- main = initialGuessRandom >>= print
-- main = initialGuessPairs >>= print
-- main = initialGuessQuads >>= print
-- main = initialGuessAllDifferent >>= print
-- main = main1
-- main = main2 solve1
-- main = main2 solve2
-- main = main3 solve1 10000
main = main3 solve2 10000

-- single step debugging
main1 :: IO ()
main1 = do
  secret <- newSecret
  putStrLn $ "secret = " ++ show secret

  --guess <- initialGuessRandom
  guess <- initialGuessPairs
  putStrLn $ "guess = " ++ show guess

  let ans = answers guess secret
  putStrLn $ "ans = " ++ show ans

  let state = (guess, ans)
  putStrLn $ "state = " ++ show state

  let legal = legalPossibilities state allPossibilities
  putStrLn $ "legal = " ++ show legal

  let guess2 = head legal
  putStrLn $ "guess2 = " ++ show guess2

  let ans2 = answers guess2 secret
  putStrLn $ "ans2 = " ++ show ans2

  let state2 = (guess2, ans2)
  putStrLn $ "state2 = " ++ show state2

  let legal2 = legalPossibilities state2 legal
  putStrLn $ "legal2 = " ++ show legal2

  let guess3 = head legal2
  putStrLn $ "guess3 = " ++ show guess3

  let ans3 = answers guess3 secret
  putStrLn $ "ans3 = " ++ show ans3

  let state3 = (guess3, ans3)
  putStrLn $ "state3 = " ++ show state3

  let legal3 = legalPossibilities state3 legal2
  putStrLn $ "legal3 = " ++ show legal3

-- Solve one secret
main2 :: ([Peg] -> [Peg] -> ([Peg], Int)) -> IO ()
main2 solv = do
  res <- solveIt solv
  putStrLn $ "res = " ++ show res

-- Run cnt solve steps and calculate the average number of iterations
main3 ::([Peg] -> [Peg] -> ([Peg], Int)) -> Int -> IO ()
main3 solv cnt = do
  res <- map snd <$> replicateM cnt (solveIt solv)
  putStrLn $ "mean = " ++ show (mean res)
    ++ ", sd = " ++ show (sd res)
    ++ ", min = " ++ show (minimum res)
    ++ ", max = " ++ show (maximum res)

-- Create a random secret and solve it
solveIt :: ([Peg] -> [Peg] -> ([Peg], Int)) -> IO ([Peg], Int)
solveIt solv = do
  secret <- newSecret
  -- guess <- initialGuessRandom        -- solve2: mean = 4.94, min = 1, max = 9
  guess <- initialGuessPairs         -- solve2: mean = 4.97, min = 1, max = 8
  -- guess <- initialGuessQuads         -- solve2: mean = 5.76, min = 1, max = 9
  -- guess <- initialGuessAllDifferent  -- solve2: mean = 4.90, min = 1, max = 9
  return $ solv guess secret
