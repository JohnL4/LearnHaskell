module KnightMovesSpec where

import Test.Hspec
import KnightMoves

import Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "various implementations" $ do
    it "moveKnight == mkw" $
      (Set.fromList $ moveKnight (5,5)) == (Set.fromList $ mkw (5,5))
    it "mkw == mkw2" $
      (Set.fromList $ mkw (5,5)) == (Set.fromList $ mkw2 (5,5))
    it "in3 == in32" $
      (Set.fromList $ in3 (5,5)) == (Set.fromList $ in32 (5,5))
