module CommandsSpec (tests) where

import Test.Hspec
import Core
import Commands

isOkay (Right _) = True
isOkay _ = False

place stone (x', y') = Place stone (Pos {x=x', y=y'})

tests =
  describe "Commands" $
    describe "when placing a stone" $ do
      it "yields a new state" $
        fst <$> execute (place Black (5, 4)) emptyGame `shouldBe` Right (withTurn White $ gameOf [((5, 4), Black)])

      it "results in events" $
        snd <$> execute (place Black (5, 4)) emptyGame `shouldBe` Right [StonePlaced Black (Pos {x=5, y=4})]

      it "flips the turn to the other player" $
        turn . fst <$> execute (place Black (5, 4)) emptyGame `shouldBe` Right White

      it "cannot be placed atop an existing stone" $
        execute (place White (1, 1)) (gameOf [((1, 1), Black)]) `shouldBe` Left LocationAlreadyOccupied

      it "cannot be placed out of turn" $
        execute (place White (1, 1)) emptyGame `shouldBe` Left OutOfTurn

      it "cannot be placed out of bounds" $ do
        execute (place Black (-1, 1)) emptyGame `shouldBe` Left OutOfBounds
        execute (place Black (1, 19)) emptyGame `shouldBe` Left OutOfBounds

      it "placement is zero-based in index" $
        isOkay $ execute (place Black (0, 0)) emptyGame
