module CoreSpec (tests) where

import Core
import Data.Foldable (for_)
import Test.Hspec

stonePlaced stone = StonePlaced stone . uncurry Pos

tests :: SpecWith ()
tests =
  describe "Core" $ do
    describe "a standard game" $ do
      it "is 19x19" $
        size emptyGame `shouldBe` 19

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

    describe "liberties" $ do
      describe "a stone in the middle of the board, all alone" $
        it "has 4 liberties" $
          liberties Black (Pos 5 5) emptyGame `shouldBe` 4

      describe "a stone in the corner" $
        it "has 2 liberties" $
          for_ [(0, 0), (18, 18), (0, 18), (18, 0)] $ \(x', y') ->
            liberties Black (Pos x' y') emptyGame `shouldBe` 2

      describe "a stone placed next to a stone of the opposing color" $
        it "loses a liberty" $
          for_ [(Black, White), (White, Black)] $ \(toPlace, enemy) ->
            for_ [(5, 6), (6, 5), (5, 4), (4, 5)] $ \(x', y') ->
              liberties toPlace (Pos x' y') (gameOf [((5, 5), enemy)]) `shouldBe` 3
