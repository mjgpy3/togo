import Test.Hspec

import qualified CommandsSpec
import qualified CoreSpec
import qualified RenderSpec

main :: IO ()
main = hspec $ do
  CommandsSpec.tests
  CoreSpec.tests
  RenderSpec.tests
