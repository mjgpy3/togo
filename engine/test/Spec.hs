import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Core
import Render
import Commands

instance Arbitrary Stone where
  arbitrary = elements [Black, White]

instance Arbitrary GameSize where
  arbitrary = pure Standard

tryHead :: [a] -> Maybe a
tryHead (v:_) = Just v
tryHead [] = Nothing

main :: IO ()
main = hspec $ do
  describe "Core" $ do
    describe "a standard game" $ do
      it "is 13x13" $
        widthAndHeight emptyGame `shouldBe` (13, 13)

      it "starts with black" $
        turn emptyGame `shouldBe` Black

    describe "summarize" $ do
      it "summarizes no events as an empty game" $
        summarize [] `shouldBe` emptyGame

      it "summarizes a single event, flipping the turn" $
        turn (summarize [StonePlaced Black (1, 1)]) `shouldBe` White

  describe "Render" $
    describe "render" $ do
      describe "given any game" $
        it "renders a board of the correct size" $
          property $ \game -> length (render game) `shouldBe` 13*13 + 13*12 + 13

      describe "given an empty game" $ do
        it "renders no stones" $
          all (`elem` "+\n-") $ render emptyGame

        it "begins with a plus sign indicating an empty slot" $
          tryHead (render emptyGame) `shouldBe` Just '+'

      it "renders the correct number of stones" $ do
        let game = gameOf [((1, 1), Black), ((2, 1), White), ((5, 8), Black), ((6, 8), White), ((5, 11), Black), ((6, 9), Black)]

        length (filter (== blackPiece) $ render game) `shouldBe` 4
        length (filter (== whitePiece) $ render game) `shouldBe` 2

      it "renders using zero-based indexes" $ do
        let game = gameOf [((0, 0), Black)]

        tryHead (render game) `shouldBe` Just blackPiece

  describe "Commands" $
    describe "execute" $ do
      it "results in a new state" $
        fst <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right (withTurn White $ gameOf [((5, 4), Black)])

      it "results in events" $
        snd <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right [StonePlaced Black (5, 4)]

      it "flips the turn to the other player" $
        turn . fst <$> execute (Place Black (5, 4)) emptyGame `shouldBe` Right White
