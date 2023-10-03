module Random where

import Control.Monad
import Mastermind
import System.Random.Stateful

newSecret :: IO [Peg]
newSecret = replicateM 4 $ randomRIO colorRange

initialGuessRandom :: IO [Peg]
initialGuessRandom = newSecret

initialGuessPairs :: IO [Peg]
initialGuessPairs = do
  r1 <- randomRIO colorRange
  r2 <- randomRIO colorRange
  if r1 /= r2
    then return [r1, r1, r2, r2]
    else initialGuessPairs

initialGuessQuads :: IO [Peg]
initialGuessQuads = do
  r <- randomRIO colorRange
  return [r, r, r, r]

initialGuessAllDifferent :: IO [Peg]
initialGuessAllDifferent = do
  [r1, r2, r3, r4] <- newSecret
  if r1 /= r2
    && r1 /= r3
    && r1 /= r4
    && r2 /= r3
    && r2 /= r4
    && r3 /= r4
    then return [r1, r2, r3, r4]
    else initialGuessAllDifferent
