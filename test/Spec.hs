-- import Spec.Codegen
import Spec.Interpreter
import Spec.Parser
import Spec.Printer
import Spec.Quickcheck

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  hspec $ do
  -- testCodegen
    testInterpreter
    testParser
    testPrinter

  -- verboseCheck temp2
