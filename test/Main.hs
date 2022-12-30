import Interpreter
import Parser
import Print.HTree (testPrintHTree)
import Print.Repr (testPrintRepr)
import Print.Show (testPrintShow)
import Print.VTree (testPrintVTree)
import Typechecker (testTypechecker)
-- import Quickcheck (temp2)

import Test.Hspec (hspec)
-- import Test.QuickCheck (verboseCheck)

main :: IO ()
main = do
  hspec $ do
    testInterpreter
    testParser
    testPrintHTree
    testPrintRepr
    testPrintShow
    testPrintVTree
    testTypechecker

  -- verboseCheck temp2
