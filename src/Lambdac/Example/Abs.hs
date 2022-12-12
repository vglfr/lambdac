{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Example.Abs where

import Lambdac.Syntax
import Lambdac.Typechecker

{- λx.xxx -> λx.xxx

  λ
 / \
x   @
   / \
  @   x
 / \
x   x
-}
f1 :: Expr
f1 = λ "x" ("x" ∘ "x" ∘ "x")

f1' :: TExpr
f1' = λ' 1 (1 • 1 • 1)

f1p :: String
f1p = "λx.xxx"

f1r :: String
f1r = "(Abs (Var \"x\") (App (App (Var \"x\") (Var \"x\")) (Var \"x\")))"

f1v :: String
f1v = "\955\n\ESC[30m\9500 \ESC[mx\n\ESC[30m\9492 \ESC[m@\n\ESC[30m  \9500 \ESC[mx @ x\n\ESC[30m  \9492 \ESC[mx\n"

f1h :: String
f1h = "  \955\n\ESC[30m / \\\ESC[m\nx   @\n\ESC[30m   / \\\ESC[m\n  @   x\n\ESC[30m / \\\ESC[m\nx   x\n\ESC[30m\ESC[m"

{- λxyz.x -> λxyz.x

  λ
 / \
x   λ
   / \
  y   λ
     / \
    z   x
-}
f2 :: Expr
f2 = λ "x" (λ "y" (λ "z" "x"))

f2' :: TExpr
f2' = λ' 1 (λ' 2 (λ' 3 1))

{- λxyz.xyz -> λxyz.xyz

  λ
 / \
x   λ
   / \
  y   λ
     / \
    z   @
       / \
      @   z
     / \
    x   y
-}
f3 :: Expr
f3 = λ "x" (λ "y" (λ "z" ("x" ∘ "y" ∘ "z")))

f3' :: TExpr
f3' = λ' 1 (λ' 2 (λ' 3 (1 • 2 • 3)))

-- λxy.xy
-- λxy.xz
-- λxy.zx
-- λxy.xxy
-- λxy.(xy)zxy
-- λxyz.zx
-- λxyz.(xy)zx
-- λxyz.(xy)zxy
-- λxyz.(λu.v)yz
-- λxyz.x(λu.v)z
-- λxyz.xy(λu.v)
