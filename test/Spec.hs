import Spec.Codegen
import Spec.Interpreter
import Spec.Parser
import Spec.Printer

import Test.Hspec

main :: IO ()
main = hspec $ do
  testCodegen
  testInterpreter
  testParser
  testPrinter

--{-# LANGUAGE OverloadedStrings #-}
-- main :: IO Expr
-- main = do
  -- eval "x"
  -- eval $ λ "x" "x" ⋅ "y"
  -- eval $ λ "x" "z" ⋅ "y"
  -- eval $ λ "x" (λ "y" "x") ⋅ "u"
  -- eval $ λ "x" (λ "y" "x") ⋅ "u" ⋅ "v"
  -- eval $ λ "x" (λ "y" "y") ⋅ "u"
  -- eval $ λ "x" (λ "y" "y") ⋅ "u" ⋅ "v"
  -- eval $ λ "x" (λ "y" "y") ⋅ "u" ⋅ "v" ⋅ "w"
  -- eval $ λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "v" ⋅ "z")
  -- eval $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")
  -- eval $ λ "x" ("x" ⋅ "x") ⋅ λ "y" ("y" ⋅ "y")
  -- eval $ λ "x" "x" ⋅ "x"
  -- eval $ λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "y" ⋅ "z")

  -- putStr "y -- " >> print (λ "x" "y")
  -- print $ isBound "y" (λ "x" "y")
  -- putStr "x -- " >> print (λ "x" "x")
  -- print $ isBound "x" (λ "x" "x")
  -- putStr "x -- " >> print (λ "x" (λ "y" "x") ⋅ "y")
  -- print $ isBound "x" (λ "x" (λ "y" "x") ⋅ "y")
  -- putStr "y -- " >> print (λ "x" (λ "y" "x") ⋅ "y")
  -- print $ isBound "y" (λ "x" (λ "y" "x") ⋅ "y")
  -- putStr "y -- " >> print (λ "x" (λ "z" "x") ⋅ "y")
  -- print $ isBound "y" (λ "x" (λ "z" "x") ⋅ "y")

  -- putStrLn ""

  -- putStr "x - " >> print (λ "x" (λ "z" "x"))
  -- print $ rename "x" (λ "x" (λ "y" "x"))

  -- putStrLn ""

  -- putStr (show $ λ "x" "x") >> putStr " " >> print (λ "x" "x")
  -- print $ isAlpha (λ "x" "x") (λ "x" "x")
  -- putStr (show $ λ "x" "x") >> putStr " " >> print (λ "x" "y")
  -- print $ isAlpha (λ "x" "x") (λ "x" "y")
  -- putStr (show $ λ "x" "y") >> putStr " " >> print (λ "z" "y")
  -- print $ isAlpha (λ "x" "y") (λ "z" "y")
  -- putStr (show $ λ "x" "z") >> putStr " " >> print (λ "z" "y")
  -- print $ isAlpha (λ "x" "z") (λ "z" "y")
  -- putStr (show $ λ "x" "x") >> putStr " " >> print (λ "y" "y")
  -- print $ isAlpha (λ "x" "x") (λ "y" "y")
  -- putStr (show $ λ "x" ("x" ⋅ "x")) >> putStr " " >> print (λ "y" ("y" ⋅ "y"))
  -- print $ isAlpha (λ "x" ("x" ⋅ "x")) (λ "y" ("y" ⋅ "y"))
  -- putStr (show $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")) >> putStr " " >> print (λ "x" ("x" ⋅ "x") ⋅ λ "y" ("y" ⋅ "y"))
  -- print $ isAlpha (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")) (λ "x" ("x" ⋅ "x") ⋅ λ "y" ("y" ⋅ "y"))

  -- print $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")
  -- print $ var (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x"))

  -- print $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")
  -- print $ bound (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x"))

  -- print $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")
  -- print $ free (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x"))

  -- pure "x"