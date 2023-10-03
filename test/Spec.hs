module Main (main) where

import Mastermind
import Solve
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Basic Tests" $ do
    it "check 001" $
      answers [1, 1, 2, 2] [1, 2, 3, 4] `shouldBe` (1, 1)

    it "check 002" $
      answers [3, 3, 4, 4] [1, 2, 3, 4] `shouldBe` (1, 1)

    it "check 003" $
      answers [4, 3, 2, 1] [1, 2, 3, 4] `shouldBe` (0, 4)

    it "check 004" $
      answers [1, 2, 3, 4] [1, 2, 3, 4] `shouldBe` (4, 0)

    it "check 005" $
      answers [5, 6, 5, 6] [1, 2, 3, 4] `shouldBe` (0, 0)

    it "check 006" $
      answers [1, 2, 3, 4] [1, 1, 2, 2] `shouldBe` (1, 1)

    it "check 007" $
      answers [1, 2, 3, 4] [3, 1, 4, 3] `shouldBe` (0, 3)

    it "check 008" $
      answers [2, 1, 3, 4] [1, 2, 3, 4] `shouldBe` (2, 2)

  describe "Advanced Tests" $ do
    it "check 101" $
      legalPossibilities ([1, 1, 2, 2], (4, 0)) allPossibilities `shouldBe` [[1, 1, 2, 2]]

    it "check 102" $
      legalPossibilities
        ([1, 1, 2, 3], (0, 4))
        allPossibilities
        `shouldBe` [ [2, 3, 1, 1],
                     [3, 2, 1, 1]
                   ]

    it "check 103" $
      legalPossibilities
        ([1, 2, 3, 4], (2, 2))
        allPossibilities
        `shouldBe` [ [1, 2, 4, 3],
                     [1, 3, 2, 4],
                     [1, 4, 3, 2],
                     [2, 1, 3, 4],
                     [3, 2, 1, 4],
                     [4, 2, 3, 1]
                   ]

  describe "Super Advanced Tests" $ do
    it "check 201" $
      legalPossibilities
        ([1, 2, 1, 2], (0, 4))
        [ [1, 2, 1, 2],
          [1, 2, 2, 1],
          [2, 1, 1, 2],
          [2, 1, 2, 1]
        ]
        `shouldBe` [[2, 1, 2, 1]]

  describe "Solve puzzle" $ do
    it "solve1" $ property prop_solve1

    it "solve2" $ property (withMaxSuccess 10000 prop_solve2)

prop_solve1 =
  forAll genPegs $ \guess ->
  forAll genPegs $ \secret ->
  let res = solve1 guess secret
  in counterexample ("res = " ++ show res) $ fst res == secret && snd res <= snd colorRange ^ 4

prop_solve2 =
  forAll genPegs $ \guess ->
  forAll genPegs $ \secret ->
  let res = solve2 guess secret
  in counterexample ("res = " ++ show res) $ fst res == secret && snd res <= 9

genPegs :: Gen [Peg]
genPegs = vectorOf 4 (choose colorRange)

genPairs :: Gen [Peg]
genPairs = do
  a <- choose colorRange
  b <- choose colorRange
  return [a, a, b, b]
