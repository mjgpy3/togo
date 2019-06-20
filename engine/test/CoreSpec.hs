module CoreSpec (tests) where

import Test.Hspec
import Core

tests =
  describe "Core" $ do
    describe "a standard game" $ do
      it "is 13x13" $
        widthAndHeight emptyGame `shouldBe` (13, 13)

      it "starts with black" $
        turn emptyGame `shouldBe` Black

    describe "summarize" $ do
      it "summarizes no events as an empty game" $
        summarize [] `shouldBe` emptyGame

      it "summarizes a single event, flipping the turn" $
        turn (summarize [StonePlaced Black (1, 1)]) `shouldBe` White
