import Lambdac.Interpreter (evalPrint)
import Lambdac.Parser (Repl (..), parseLine)
import Lambdac.Print.VTree (vtree)
import Lambdac.Print.Repr (repr)

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
      NOP -> pure ()
    pure x
