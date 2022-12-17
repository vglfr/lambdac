{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Example.Abs where

import Lambdac.Syntax (Expr, TExpr, λ, λ', (∘), (•))

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

f1e :: Expr
f1e = λ "x" ("x" ∘ "x" ∘ "x")

f1t :: TExpr
f1t = undefined

f1p :: String
f1p = "λx.xxx"

f1r :: String
f1r = "(Abs (Var \"x\") (App (App (Var \"x\") (Var \"x\")) (Var \"x\")))"

f1v :: String
f1v = "\955\n\ESC[30m\9500 \ESC[mx\n\ESC[30m\9492 \ESC[m@\n\ESC[30m  \9500 \ESC[mx @ x\n\ESC[30m  \9492 \ESC[mx"

f1h :: String
f1h = "  \955\n\ESC[30m / \\\ESC[m\nx   @\n\ESC[30m   / \\\ESC[m\n  @   x\n\ESC[30m / \\\ESC[m\nx   x"

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

f2e :: Expr
f2e = undefined

f2t :: TExpr
f2t = undefined

f2p :: String
f2p = "λxyz.x"

f2r :: String
f2r = undefined

f2v :: String
f2v = undefined

f2h :: String
f2h = undefined

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

f3e :: Expr
f3e = undefined

f3t :: TExpr
f3t = undefined

f3p :: String
f3p = "λxyz.xyz"

f3r :: String
f3r = undefined

f3v :: String
f3v = undefined

f3h :: String
f3h = undefined

{- λx.x -> λx.x

  λ
 / \
x   x
-}
f4 :: Expr
f4 = λ "x" "x"

f4' :: TExpr
f4' = λ' 0 0

f4e :: Expr
f4e = λ "x" "x"

f4t :: TExpr
f4t = undefined

f4p :: String
f4p = "λx.x"

f4r :: String
f4r = undefined

f4v :: String
f4v = undefined

f4h :: String
f4h = undefined

{- λx.(λy.y)x -> λx.x

  λ
 / \
x   @
   / \
  λ   x
 / \
y   y
-}
f5 :: Expr
f5 = λ "x" (λ "y" "y" ∘ "x")

f5' :: TExpr
f5' = λ' 0 0

f5e :: Expr
f5e = λ "x" "x"

f5t :: TExpr
f5t = undefined

f5p :: String
f5p = undefined

f5r :: String
f5r = undefined

f5v :: String
f5v = undefined

f5h :: String
f5h = undefined

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
