import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Core

main :: IO ()
main = hspec $ do
  describe "Core" $
    describe "summarize" $
      it "summarizes no events as an empty game" $ do
        summarize [] == emptyGame
