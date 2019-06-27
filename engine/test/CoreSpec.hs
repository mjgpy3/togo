module CoreSpec (tests) where

import Core
import Data.Foldable (for_)
import Test.Hspec
import qualified Data.Set as S
import Render (render)
import Util

empty = summarize emptyGame

tests :: SpecWith ()
tests =
  describe "Core" $ do
    describe "a standard game" $ do
      it "is 19x19" $
        size empty `shouldBe` 19

      it "starts with black" $
        turn empty `shouldBe` Black

      it "has no captures" $ do
        stonesCapturedBy Black empty `shouldBe` 0
        stonesCapturedBy White empty `shouldBe` 0

    describe "summarize" $ do
      it "summarizes no events as an empty game" $
        summarize [] `shouldBe` empty

      it "yields a new state when a stone is placed" $
        summarize [stonePlaced Black (5, 4)] `shouldBe` withTurn White (summarize $ gameOf [((5, 4), Black)])

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
          S.size (liberties Black (Pos 5 5) empty) `shouldBe` 4

      describe "a stone in the corner" $
        it "has 2 liberties" $
          for_ [(0, 0), (18, 18), (0, 18), (18, 0)] $ \(x', y') ->
            S.size (liberties Black (Pos x' y') empty) `shouldBe` 2

      describe "a stone placed next to a stone of the opposing color" $
        it "loses a liberty" $
          for_ [(Black, White), (White, Black)] $ \(toPlace, enemy) ->
            for_ [(5, 6), (6, 5), (5, 4), (4, 5)] $ \(x', y') ->
              S.size (liberties toPlace (Pos x' y') (summarize $ gameOf [((5, 5), enemy)])) `shouldBe` 3

      describe "a stone placed next to a stone of the same color" $
        it "loses a liberty but gains its neighbors liberties" $
          for_ [Black, White] $ \toPlace ->
            for_ [(5, 6), (6, 5), (5, 4), (4, 5)] $ \(x', y') ->
              S.size (liberties toPlace (Pos x' y') (summarize $ gameOf [((5, 5), toPlace)])) `shouldBe` 6

      describe "a few more complicated placements" $ do
        describe "more than two stones together" $
          it "tracks all liberties without double counting" $
            S.size (liberties Black (Pos 5 5) (summarize $ Black `atPlaces` [(4, 5), (5, 5), (6, 5)])) `shouldBe` 8

        describe "a stone in the middle of a diamond" $
          it "tracks all liberties without double counting" $
            S.size (liberties Black (Pos 5 5) (summarize $ Black `atPlaces` [(5, 4), (4, 5), (5, 5), (6, 5), (5, 6)])) `shouldBe` 8

        describe "effectively dead pieces" $
          it "can still have liberties" $ do
            let game = summarize $ gameOf [ ((0, 0), Black)
                              , ((2, 0), White)
                              , ((2, 1), Black)
                              , ((0, 1), White)
                              , ((10, 10), Black)
                              , ((1, 1), White)
                              ]
            S.size (liberties Black (Pos 0 0) game) `shouldBe` 1
            stoneAt (Pos 0 0) game `shouldBe` Just Black

    describe ("collectCaptures when " ++ show Black ++ " captures " ++ show White)  $ do
      describe "given an empty game" $
        it "makes no change" $
          collectCaptures empty `shouldBe` empty

      describe "given a board with a simple capture" $ do
        let piecesBeforeCapture = [ ((0, 1), Black)
                                  , ((10, 10), White)
                                  , ((1, 0), Black)
                                  , ((10, 11), White)
                                  , ((2, 1), Black)
                                  , ((1, 1), White)
                                  ]
        let stateAfterCaptures = collectCaptures $ summarize $ gameOf (((1, 2), Black):piecesBeforeCapture)

        it ("looks like the following before capture\n" ++ render (summarize $ gameOf piecesBeforeCapture)) passes

        it ("looks like the following after capture\n" ++ render stateAfterCaptures) passes

        it "removes the White stone" $
          stoneAt (Pos 1 1) stateAfterCaptures `shouldBe` Nothing

        it "counts that stone as a capture for the Black's color" $
          stonesCapturedBy Black stateAfterCaptures `shouldBe` 1

        it "does not affect the White player's capture count" $
          stonesCapturedBy White stateAfterCaptures `shouldBe` 0

        it "does not remove the Black's stones" $
          for_ [Pos 0 1, Pos 1 0, Pos 1 2, Pos 2 1] $ \pos ->
            stoneAt pos stateAfterCaptures `shouldBe` Just Black

      describe "given a board with a more complicated capture" $ do
        let piecesBeforeCapture = [ ((1, 0), Black)
                                  , ((10, 10), White)
                                  , ((0, 2), Black)
                                  , ((11, 10), White)
                                  , ((1, 3), Black)
                                  , ((12, 10), White)
                                  , ((2, 1), Black)
                                  , ((1, 1), White)
                                  , ((2, 2), Black)
                                  , ((1, 2), White)
                                  ]
        let stateAfterCaptures = collectCaptures $ summarize $ gameOf (((0, 1), Black):piecesBeforeCapture)

        it ("looks like the following before capture\n" ++ render (summarize $ gameOf piecesBeforeCapture)) passes

        it ("looks like the following after capture\n" ++ render stateAfterCaptures) passes

        it "removes the White stones" $ do
          stoneAt (Pos 1 1) stateAfterCaptures `shouldBe` Nothing
          stoneAt (Pos 1 2) stateAfterCaptures `shouldBe` Nothing

        it "counts those stones as a capture for the Black's color" $
          stonesCapturedBy Black stateAfterCaptures `shouldBe` 2

        it "does not affect the White player's capture count" $
          stonesCapturedBy White stateAfterCaptures `shouldBe` 0

        it "does not remove the Black's stones" $
          for_ [Pos 1 0, Pos 0 2, Pos 1 3, Pos 2 1, Pos 2 2, Pos 0 1] $ \pos ->
            stoneAt pos stateAfterCaptures `shouldBe` Just Black
