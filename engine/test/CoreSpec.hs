module CoreSpec (tests) where

import Test.Hspec
import Core

stonePlaced stone = StonePlaced stone . uncurry Pos

tests :: SpecWith ()
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

      it "yields a new state when a stone is placed" $
        summarize [stonePlaced Black (5, 4)] `shouldBe` withTurn White (gameOf [((5, 4), Black)])

      it "summarizes a single event, flipping the turn" $
        turn (summarize [stonePlaced Black (1, 1)]) `shouldBe` White

      it "playing a stone changes the turn" $
        turn (summarize [stonePlaced Black (1, 1)]) `shouldBe` White

      it "passing changes the turn but not the board" $
        turn (summarize [TurnPassed]) `shouldBe` White

      it "a pass in response to a pass ends the game" $
        isEndGame $ summarize [TurnPassed, TurnPassed]

      it "resignation ends the game" $
        isEndGame $ summarize [PlayerResigned]
