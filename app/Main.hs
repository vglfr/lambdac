module Main where

import Lambdac.Interpreter
import Lambdac.Parser
import Lambdac.Syntax

import System.IO (hFlush, stdout)

main :: IO Expr
main = ps1 >> getLine >>= parseProg >>= evalPrint >> main
 where
  ps1 = putStr "> " >> hFlush stdout
