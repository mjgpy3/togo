module RenderSpec (tests) where

import Test.Hspec
import Test.QuickCheck
import Core
import Render

instance Arbitrary Stone where
  arbitrary = elements [Black, White]

instance Arbitrary GameSize where
  arbitrary = pure Standard

instance Arbitrary GameState where
  arbitrary = elements [InProgress, PassedInProgress, EndGame]

instance Arbitrary State where
  arbitrary = Game <$> arbitrary

tryHead :: [a] -> Maybe a
tryHead (v:_) = Just v
tryHead [] = Nothing

tests =
  describe "Render" $
    describe "render" $ do
      describe "given any game" $
        it "renders a board of the correct size" $
          property $ \game -> length (render game) `shouldBe` 19*19 + 19*18 + 19

      describe "given an empty game" $ do
        it "renders no stones" $
          all (`elem` "+\n-") $ render emptyGame

        it "begins with a plus sign indicating an empty slot" $
          tryHead (render emptyGame) `shouldBe` Just '+'

      it "renders the correct number of stones" $ do
        let game = gameOf [((1, 1), Black), ((2, 1), White), ((5, 8), Black), ((6, 8), White), ((5, 11), Black), ((6, 9), Black)]

        length (filter (== blackPiece) $ render game) `shouldBe` 4
        length (filter (== whitePiece) $ render game) `shouldBe` 2

      it "renders using zero-based indexes" $ do
        let game = gameOf [((0, 0), Black)]

        tryHead (render game) `shouldBe` Just blackPiece
