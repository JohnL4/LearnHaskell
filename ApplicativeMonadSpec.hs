module ApplicativeMonadSpec where

import Test.Hspec
import ApplicativeMonad

main :: IO ()
main = hspec $ do
  describe "land" $ do
    it "returns a Pole when not too many bird land on the left end of the pole" $
      land LeftSide 1 (JJust (5,5)) `shouldBe` (JJust (6,5))
    it "returns a Pole when not too many bird land on the right end of the pole" $
      land RightSide 1 (JJust (5,5)) `shouldBe` (JJust (5,6))
    it "returns NNothing when too many birds land on the left end of the pole" $
      land LeftSide 4 (JJust (5,5)) `shouldBe` NNothing
    it "returns NNothing when too many birds land on the right end of the pole" $
      land RightSide 4 (JJust (5,5)) `shouldBe` NNothing
  describe "take off" $ do
    it "returns a Pole when not too many birds take off from the left end" $
      land LeftSide (-3) (JJust (5,5)) `shouldBe` (JJust (2,5))
    it "returns a Pole when not too many birds take off from the right end" $
      land RightSide (-3) (JJust (5,5)) `shouldBe` (JJust (5,2))
    it "returns NNothing when too many birds take off from the left end" $
      land LeftSide (-4) (JJust (5,5)) `shouldBe` NNothing
    it "returns NNothing when too many birds take off from the right end" $
      land RightSide (-4) (JJust (5,5)) `shouldBe` NNothing
  describe "we don't care about negative numbers of birds" $ do
    it "returns a Pole with negative bird counts when too many birds take off" $
      land LeftSide (-3) (JJust (0,0)) `shouldBe` (JJust (-3,0))
  describe "chaining" $ do
    it "allows (-:)" $
      JJust (0,0) -: land LeftSide 1 `shouldBe` JJust (1,0)
    it "allows chaining" $
      JJust (0,0) -: land LeftSide 3 -: land RightSide 1 -: land LeftSide 1 `shouldBe` (JJust (4,1))
      -- JJust (0,0) -: land LeftSide 3 -: land RightSide 1 `shouldBe` JJust (3,1)
    it "returns failure in middle" $
      JJust (0,0) -: land LeftSide 3 -: land RightSide 1 -: land LeftSide 1 -: land RightSide (-1) -: land RightSide 1
      `shouldBe` NNothing
