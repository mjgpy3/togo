module CoreSpec (tests) where

import Test.Hspec
import Core

tests =
  describe "Core" $ do
    describe "a standard game" $ do
      it "is 19x19" $
        widthAndHeight emptyGame `shouldBe` (19, 19)

      it "starts with black" $
        turn emptyGame `shouldBe` Black

    describe "summarize" $ do
      it "summarizes no events as an empty game" $
        summarize [] `shouldBe` emptyGame

      it "summarizes a single event, flipping the turn" $
        turn (summarize [StonePlaced Black (1, 1)]) `shouldBe` White

      it "passing changes the turn but not the board" $
        turn (summarize [TurnPassed]) `shouldBe` White

      it "a pass in response to a pass ends the game" $
        isEndGame $ summarize [TurnPassed, TurnPassed]
