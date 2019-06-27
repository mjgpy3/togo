module CoreSpec (tests) where

import Core
import Data.Foldable (for_)
import Test.Hspec
import qualified Data.Set as S

stonePlaced stone = StonePlaced stone . uncurry Pos

atPlaces stone positions = gameOf $ map (\pos -> (pos, stone)) positions

tests :: SpecWith ()
tests =
  describe "Core" $ do
    describe "a standard game" $ do
      it "is 19x19" $
        size emptyGame `shouldBe` 19

      it "starts with black" $
        turn emptyGame `shouldBe` Black

      it "has no captures" $ do
        stonesCapturedBy Black emptyGame `shouldBe` 0
        stonesCapturedBy White emptyGame `shouldBe` 0

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
          S.size (liberties Black (Pos 5 5) emptyGame) `shouldBe` 4

      describe "a stone in the corner" $
        it "has 2 liberties" $
          for_ [(0, 0), (18, 18), (0, 18), (18, 0)] $ \(x', y') ->
            S.size (liberties Black (Pos x' y') emptyGame) `shouldBe` 2

      describe "a stone placed next to a stone of the opposing color" $
        it "loses a liberty" $
          for_ [(Black, White), (White, Black)] $ \(toPlace, enemy) ->
            for_ [(5, 6), (6, 5), (5, 4), (4, 5)] $ \(x', y') ->
              S.size (liberties toPlace (Pos x' y') (gameOf [((5, 5), enemy)])) `shouldBe` 3

      describe "a stone placed next to a stone of the same color" $
        it "loses a liberty but gains its neighbors liberties" $
          for_ [Black, White] $ \toPlace ->
            for_ [(5, 6), (6, 5), (5, 4), (4, 5)] $ \(x', y') ->
              S.size (liberties toPlace (Pos x' y') (gameOf [((5, 5), toPlace)])) `shouldBe` 6

      describe "a few more complicated placements" $ do
        describe "more than two stones together" $
          it "tracks all liberties without double counting" $
            S.size (liberties Black (Pos 5 5) (Black `atPlaces` [(4, 5), (5, 5), (6, 5)])) `shouldBe` 8

        describe "a stone in the middle of a diamond" $
          it "tracks all liberties without double counting" $
            S.size (liberties Black (Pos 5 5) (Black `atPlaces` [(5, 4), (4, 5), (5, 5), (6, 5), (5, 6)])) `shouldBe` 8

    describe "collectCaptures" $ do
      describe "given an empty game" $
        it "makes no change" $
          collectCaptures emptyGame `shouldBe` emptyGame

      describe "given a board with a simple capture" $ do
        let stateAfterCaptures = collectCaptures $ gameOf [ ((0, 1), White)
                                                          , ((1, 0), White)
                                                          , ((2, 1), White)
                                                          , ((1, 2), White)
                                                          , ((1, 1), Black)
                                                          ]

        it "removes the stone that was captured" $
          stoneAt (Pos 1 1) stateAfterCaptures `shouldBe` Nothing

        it "counts that stone as a capture for the capturing color" $
          stonesCapturedBy White stateAfterCaptures `shouldBe` 1
