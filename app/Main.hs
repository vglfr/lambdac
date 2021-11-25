module Main where

import Lambdac.Interpreter
import Lambdac.Parser
import Lambdac.Printer
import Lambdac.Syntax

import System.IO (hFlush, stdout)

main :: IO Repl
main = ps1 >> getLine >>= parseLine >>= exec >> main
-- main = ps1 >> getLine >>= parseProg >>= evalPrint >> main
 where
  ps1 = putStr "> " >> hFlush stdout
  exec x = case x of
    Tree e -> putStrLn (tree e) >> pure x
    Repr e -> putStrLn (repr e) >> pure x
    Eval e -> print e >> pure x
    -- Eval e -> evalPrint e >> pure x
