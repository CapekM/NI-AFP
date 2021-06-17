import Test.Hspec

import qualified LoggingSpec
import qualified StackSpec
import qualified StackMachineSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Logging"      LoggingSpec.spec
  describe "Data.Stack"   StackSpec.spec
  describe "StackMachine" StackMachineSpec.spec
