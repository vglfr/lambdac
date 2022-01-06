import Lambdac.Interpreter
import Lambdac.Parser
import Lambdac.Printer
import Lambdac.Syntax

import System.IO (hFlush, stdout)

main :: IO Repl
main = ps1 >> getLine >>= parseLine >>= exec >> main
 where
  ps1 = putStr "> " >> hFlush stdout
  exec x = do
    case x of
      PPrint e -> print e
      Tree e -> putStrLn (vtree e)
      Repr e -> putStrLn (repr e)
      Eval e -> evalPrint e
    pure x
