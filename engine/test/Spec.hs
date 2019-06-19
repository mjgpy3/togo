import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Core

main :: IO ()
main = hspec $ do
  describe "Core" $
    describe "summarize" $ do
      it "summarizes no events as an empty game" $
        summarize [] == emptyGame

      it "summarizes a single event" $
        summarize [StonePlaced Black (1, 1)] == gameOf [((1, 1), Black)]
