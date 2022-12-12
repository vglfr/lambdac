-- import Spec.Codegen
import Spec.Interpreter
import Spec.Parser
import Spec.Print.HTree (testPrintHTree)
import Spec.Print.Repr (testPrintRepr)
import Spec.Print.Show (testPrintShow)
import Spec.Print.VTree (testPrintVTree)
import Spec.Quickcheck

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  hspec $ do
  -- testCodegen
    testInterpreter
    testParser
    testPrintHTree
    testPrintRepr
    testPrintShow
    testPrintVTree

  -- verboseCheck temp2
