import Test.Hspec

import qualified ShapesSpec
import qualified SortedListSpec
import qualified RomanNumeralSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Shapes"       ShapesSpec.spec
  describe "SortedList"   SortedListSpec.spec
  describe "RomanNumeral" RomanNumeralSpec.spec
