module CommandsSpec (tests) where

import Test.Hspec
import Core
import Commands
import Util

place stone = Place stone . uncurry Pos

tests :: SpecWith ()
tests =
  describe "Commands" $
    describe "when placing a stone" $ do
      it "results in an event" $
        execute (place Black (5, 4)) emptyGame `shouldBe` Right (StonePlaced Black (Pos 5 4))

      it "cannot be placed atop an existing stone" $
        execute (place White (1, 1)) (gameOf [((1, 1), Black)]) `shouldBe` Left LocationAlreadyOccupied

      it "cannot be placed out of turn" $
        execute (place White (1, 1)) emptyGame `shouldBe` Left OutOfTurn

      it "cannot be placed out of bounds" $ do
        execute (place Black (-1, 1)) emptyGame `shouldBe` Left OutOfBounds
        execute (place Black (1, 19)) emptyGame `shouldBe` Left OutOfBounds

      it "placement is zero-based in index" $
        isOkay $ execute (place Black (0, 0)) emptyGame

      it "cannot be placed when the game has been ended" $
        execute (place Black (5, 6)) endedGame `shouldBe` Left GameEnded

      it "cannot be placed if it would have no liberties (obvious)" $
        execute (place Black (0, 0)) (White `atPlaces` [(0, 1), (1, 0)]) `shouldBe` Left PlacementHasNoLiberties

      it "cannot be placed if it would have no liberties (slightly less obvious)" $ do
        let game = gameOf [ ((0, 0), Black)
                          , ((2, 0), White)
                          , ((2, 1), Black)
                          , ((0, 1), White)
                          , ((10, 10), Black)
                          , ((1, 1), White)
                          ]
        execute (place Black (1, 0)) game `shouldBe` Left PlacementHasNoLiberties
