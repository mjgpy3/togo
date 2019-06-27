module CoreSpec (tests) where

import Core
import Data.Foldable (for_)
import Test.Hspec
import qualified Data.Set as S
import Render (render)

stonePlaced stone = StonePlaced stone . uncurry Pos

atPlaces stone positions = gameOf $ map (\pos -> (pos, stone)) positions

passes = 1 `shouldBe` 1

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
        let piecesBeforeCapture = [ ((0, 1), White)
                                  , ((1, 0), White)
                                  , ((2, 1), White)
                                  , ((1, 1), Black)
                                  ]
        let stateAfterCaptures = collectCaptures $ gameOf (((1, 2), White):piecesBeforeCapture)

        it ("looks like the following before capture\n" ++ render (gameOf piecesBeforeCapture)) passes

        it ("looks like the following after capture\n" ++ render stateAfterCaptures) passes

        it "removes the captured stone" $
          stoneAt (Pos 1 1) stateAfterCaptures `shouldBe` Nothing

        it "counts that stone as a capture for the capturer's color" $
          stonesCapturedBy White stateAfterCaptures `shouldBe` 1

        it "does not affect the captured player's capture count" $
          stonesCapturedBy Black stateAfterCaptures `shouldBe` 0

        it "does not remove the capturer's stones" $ do
          for_ [Pos 0 1, Pos 1 0, Pos 1 2, Pos 2 1] $ \pos ->
            stoneAt pos stateAfterCaptures `shouldBe` Just White

      describe "given a board with a more complicated capture" $ do
        let piecesBeforeCapture = [ ((1, 0), White)
                                  , ((0, 2), White)
                                  , ((1, 3), White)
                                  , ((2, 1), White)
                                  , ((2, 2), White)
                                  , ((1, 1), Black)
                                  , ((1, 2), Black)
                                  ]
        let stateAfterCaptures = collectCaptures $ gameOf (((0, 1), White):piecesBeforeCapture)

        it ("looks like the following before capture\n" ++ render (gameOf piecesBeforeCapture)) passes

        it ("looks like the following after capture\n" ++ render stateAfterCaptures) passes

        it "removes the captured stones" $ do
          stoneAt (Pos 1 1) stateAfterCaptures `shouldBe` Nothing
          stoneAt (Pos 1 2) stateAfterCaptures `shouldBe` Nothing

        it "counts those stones as a capture for the capturer's color" $
          stonesCapturedBy White stateAfterCaptures `shouldBe` 2

        it "does not affect the captured player's capture count" $
          stonesCapturedBy Black stateAfterCaptures `shouldBe` 0

        it "does not remove the capturer's stones" $ do
          for_ [Pos 1 0, Pos 0 2, Pos 1 3, Pos 2 1, Pos 2 2, Pos 0 1] $ \pos ->
            stoneAt pos stateAfterCaptures `shouldBe` Just White

        
