module CommandsSpec (tests) where

import Test.Hspec
import Core
import Commands

isOkay :: Either a b -> Bool
isOkay (Right _) = True
isOkay _ = False

endedGame :: State
endedGame = summarize [TurnPassed, TurnPassed]

tests :: SpecWith ()
tests =
  describe "Commands" $
    describe "when placing a stone" $ do
      it "yields a new state" $
        fst <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right (withTurn White $ gameOf [((5, 4), Black)])

      it "results in events" $
        snd <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right (StonePlaced Black (5, 4))

      it "flips the turn to the other player" $
        turn . fst <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right White

      it "cannot be placed atop an existing stone" $
        execute (Place White (1, 1)) (gameOf [((1, 1), Black)]) `shouldBe` Left LocationAlreadyOccupied

      it "cannot be placed out of turn" $
        execute (Place White (1, 1)) emptyGame `shouldBe` Left OutOfTurn

      it "cannot be placed out of bounds" $ do
        execute (Place Black (-1, 1)) emptyGame `shouldBe` Left OutOfBounds
        execute (Place Black (1, 19)) emptyGame `shouldBe` Left OutOfBounds

      it "placement is zero-based in index" $
        isOkay $ execute (Place Black (0, 0)) emptyGame

      it "cannot be placed when the game has been ended" $
        execute (Place Black (5, 6)) endedGame `shouldBe` Left GameEnded
