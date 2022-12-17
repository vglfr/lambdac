import Lambdac.Interpreter (evalPrint)
import Lambdac.Parser (Command (..), parseLine)
import Lambdac.Print.HTree (htree)
import Lambdac.Print.VTree (vtree)
import Lambdac.Print.Repr (repr)

import System.IO (hFlush, stdout)

main :: IO Command
main = ps1 >> getLine >>= parseLine >>= exec >> main
 where
  ps1 = putStr "> " >> hFlush stdout
  exec x = do
    case x of
      PPrint e -> print e
      VTree e -> putStrLn $ vtree e
      HTree e -> putStrLn $ htree e
      Repr e -> putStrLn $ repr e
      Eval e -> evalPrint e
      NOP -> pure ()
    pure x
