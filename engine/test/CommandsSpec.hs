module CommandsSpec (tests) where

import Test.Hspec
import Core
import Commands

tests =
  describe "Commands" $
    describe "execute" $ do
      it "results in a new state" $
        fst <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right (withTurn White $ gameOf [((5, 4), Black)])

      it "results in events" $
        snd <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right [StonePlaced Black (5, 4)]

      it "flips the turn to the other player" $
        turn . fst <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right White
