import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "todo engine" $
    it "doesn't have much yet" $ do
      pending
