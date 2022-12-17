{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Example.App where

import Lambdac.Syntax (Expr, TExpr, λ, λ', (∘), (•))

{- x x -> x x

  @
 / \
x   x
-}
a1 :: Expr
a1 = "x" ∘ "x"

a1' :: TExpr
a1' = 0 • 0

a1e :: Expr
a1e = undefined

a1t :: TExpr
a1t = undefined

a1p :: String
a1p = "x x"

a1r :: String
a1r = "(App (Var \"x\") (Var \"x\"))"

a1v :: String
a1v = "x @ x"

a1h :: String
a1h = "  @\n\ESC[30m / \\\ESC[m\nx   x"

{- x x x -> x x x

    @
   / \
  @   x
 / \
x   x
-}
a2 :: Expr
a2 = "x" ∘ "x" ∘ "x"

a2' :: TExpr
a2' = 0 • 0 • 0

a2e :: Expr
a2e = undefined

a2t :: TExpr
a2t = undefined

a2p :: String
a2p = "x x x"

a2r :: String
a2r = undefined

a2v :: String
a2v = undefined

a2h :: String
a2h = undefined

{- λx.x x -> x

    @
   / \
  λ   x
 / \
x   x
-}
a3 :: Expr
a3 = λ "x" "x" ∘ "x"

a3' :: TExpr
a3' = λ' 1 1 • 0

a3e :: Expr
a3e = "x"

a3t :: TExpr
a3t = 0

a3p :: String
a3p = undefined

a3r :: String
a3r = undefined

a3v :: String
a3v = undefined

a3h :: String
a3h = undefined

-- x λx.x
-- λx.x λx.x
-- λx.x λy.y

{- λx.xx λy.yy -> λy.yy λy.yy

     @
   ⡀⠁ ⠈⢀
  λ     λ
 / \   / \
x   @ y   @
   / \   / \
  x   x y   y
-}
a4 :: Expr
a4 = λ "x" ("x" ∘ "x") ∘ λ "y" ("y" ∘ "y")

a4' :: TExpr
a4' = λ' 1 (1 • 1) • λ' 2 (2 • 2)

a4e :: Expr
a4e = λ "y" ("y" ∘ "y") ∘ λ "y" ("y" ∘ "y")

a4t :: TExpr
a4t = undefined

a4p :: String
a4p = undefined

a4r :: String
a4r = undefined

a4v :: String
a4v = undefined

a4h :: String
a4h = undefined

-- λx.x λx.x λx.x

{- (λx.x λy.y) (λz.z u) -> u

           @
       ⡀ ⠁   ⠈ ⢀
     @           @
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀
  λ     λ     λ     u
 ⡈ ⢁   ⡈ ⢁   ⡈ ⢁
x   x y   y z   z
-}
a5 :: Expr
a5 = (λ "x" "x" ∘ λ "y" "y") ∘ (λ "z" "z" ∘ "u")

a5' :: TExpr
a5' = (λ' 1 1 • λ' 2 2) • (λ' 3 3 • 0)

a5e :: Expr
a5e = undefined

a5t :: TExpr
a5t = undefined

a5p :: String
a5p = undefined

a5r :: String
a5r = undefined

a5v :: String
a5v = undefined

a5h :: String
a5h = undefined

-- (λx.x)(λy.y)z
-- ((λx.x)(λy.y))z
-- (λxy.xxy)(λx.xy)(λx.xz)
-- (λxyz.xz(yz))(λnn.n)(λp.p)
-- (λy.λz(λn.λn.n)z(yz))(λp.p)
-- λz(λn.z)((λp.p)z)

{- λx.xxx z -> z z z

    @
   / \
  λ   z
 / \
x   @
   / \
  @   x
 / \
x   x
-}
a6 :: Expr
a6 = λ "x" ("x" ∘ "x" ∘ "x") ∘ "z"

a6' :: TExpr
a6' = λ' 1 (1 • 1 • 1) • 0

a6e :: Expr
a6e = undefined

a6t :: TExpr
a6t = undefined

a6p :: String
a6p = undefined

a6r :: String
a6r = undefined

a6v :: String
a6v = undefined

a6h :: String
a6h = undefined

{- λx.x y -> y

    @
   / \
  λ   y
 / \
x   x
-}
a7 :: Expr
a7 = λ "x" "x" ∘ "y"

a7' :: TExpr
a7' = λ' 1 1 • 0

a7e :: Expr
a7e = "y"

a7t :: TExpr
a7t = 0

a7p :: String
a7p = undefined

a7r :: String
a7r = undefined

a7v :: String
a7v = undefined

a7h :: String
a7h = undefined

{- λx.z y -> z

    @
   / \
  λ   y
 / \
x   z
-}
a8 :: Expr
a8 = λ "x" "z" ∘ "y"

a8' :: TExpr
a8' = λ' 1 2 • 0

a8e :: Expr
a8e = "z"

a8t :: TExpr
a8t = 2

a8p :: String
a8p = undefined

a8r :: String
a8r = undefined

a8v :: String
a8v = undefined

a8h :: String
a8h = undefined

{- λxy.x u -> λy.u

    @
   / \
  λ   u
 / \
x   λ
   / \
  y   x
-}
a9 :: Expr
a9 = λ "x" (λ "y" "x") ∘ "u"

a9' :: TExpr
a9' = undefined

a9e :: Expr
a9e = λ "y" "u"

a9t :: TExpr
a9t = undefined

a9p :: String
a9p = undefined

a9r :: String
a9r = undefined

a9v :: String
a9v = undefined

a9h :: String
a9h = undefined

{- λxy.y u -> λy.y

    @
   / \
  λ   y
 / \
x   z
-}
a10 :: Expr
a10 = λ "x" (λ "y" "y") ∘ "u"

a10' :: TExpr
a10' = undefined

a10e :: Expr
a10e = λ "y" "y"

a10t :: TExpr
a10t = undefined

a10p :: String
a10p = undefined

a10r :: String
a10r = undefined

a10v :: String
a10v = undefined

a10h :: String
a10h = undefined

{- λxy.xy λz.(λc.v)z -> λy.v

       @
    ⡀⠁   ⠈⢀
  λ         λ
 / \       / \
x   λ     z   @
   / \       / \
  y   @     λ   z
     / \   / \ 
    x   y c   v
-}
a11 :: Expr
a11 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "z" (λ "c" "v" ∘ "z")

a11' :: TExpr
a11' = undefined

a11e :: Expr
a11e = λ "y" "v"

a11t :: TExpr
a11t = undefined

a11p :: String
a11p = undefined

a11r :: String
a11r = undefined

a11v :: String
a11v = undefined

a11h :: String
a11h = undefined

{- λxy.xy λz.(λc.y)z -> λy'.y

       @
    ⡀⠁   ⠈⢀
  λ         λ
 / \       / \
x   λ     z   @
   / \       / \
  y   @     λ   z
     / \   / \ 
    x   y c   y
-}
a12 :: Expr
a12 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "z" (λ "c" "y" ∘ "z")

a12' :: TExpr
a12' = undefined

a12e :: Expr
a12e = λ "y'" "y"

a12t :: TExpr
a12t = undefined

a12p :: String
a12p = undefined

a12r :: String
a12r = undefined

a12v :: String
a12v = undefined

a12h :: String
a12h = undefined

{- λx.xx λx.xx -> λx.xx λx.xx

     @
   ⡀⠁ ⠈⢀
  λ     λ
 / \   / \
x   @ y   @
   / \   / \
  x   x y   y
-}
a13 :: Expr
a13 = λ "x" ("x" ∘ "x") ∘ λ "x" ("x" ∘ "x")

a13' :: TExpr
a13' = undefined

a13e :: Expr
a13e = λ "x" ("x" ∘ "x") ∘ λ "x" ("x" ∘ "x")

a13t :: TExpr
a13t = undefined

a13p :: String
a13p = undefined

a13r :: String
a13r = undefined

a13v :: String
a13v = undefined

a13h :: String
a13h = undefined

{- λxy.x u v -> u

      @
     / \
    @   v
   / \
  λ   u
 / \
x   λ
   / \
  y   x
-}
a15 :: Expr
a15 = λ "x" (λ "y" "x") ∘ "u" ∘ "v"

a15' :: TExpr
a15' = undefined

a15e :: Expr
a15e = "u"

a15t :: TExpr
a15t = undefined

a15p :: String
a15p = undefined

a15r :: String
a15r = undefined

a15v :: String
a15v = undefined

a15h :: String
a15h = undefined

{- λxy.y u v -> v

      @
     / \
    @   v
   / \
  λ   u
 / \
x   λ
   / \
  y   y
-}
a16 :: Expr
a16 = λ "x" (λ "y" "y") ∘ "u" ∘ "v"

a16' :: TExpr
a16' = undefined

a16e :: Expr
a16e = "v"

a16t :: TExpr
a16t = undefined

a16p :: String
a16p = undefined

a16r :: String
a16r = undefined

a16v :: String
a16v = undefined

a16h :: String
a16h = undefined

{- λxy.y u v w -> v w

        @
       / \
      @   w
     / \
    @   v
   / \
  λ   u
 / \
x   λ
   / \
  y   y
-}
a17 :: Expr
a17 = λ "x" (λ "y" "y") ∘ "u" ∘ "v" ∘ "w"

a17' :: TExpr
a17' = undefined

a17e :: Expr
a17e = "v" ∘ "w"

a17t :: TExpr
a17t = undefined

a17p :: String
a17p = undefined

a17r :: String
a17r = undefined

a17v :: String
a17v = undefined

a17h :: String
a17h = undefined

-- (λabc.cba)zz(λwv.w)
-- (λx.λy.xyy)(λa.a)b
-- (λy.y)(λx.xx)(λz.zq)
-- (λz.z)(λz.zz)(λz.zy)
-- (λx.λy.xyy)(λy.y)y
-- (λa.aa)(λb.ba)c
-- (λxyz.xz(yz))(λx.z)(λx.a)
-- λx.xxx z
