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

a2p :: String
a2p = "x x x"

a2r :: String
a2r = ""

a2v :: String
a2v = ""

a2h :: String
a2h = ""

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

-- x λx.x
-- λx.x λx.x
-- λx.x λy.y

{- (λx.xx)(λy.yy)

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

-- λx.x λx.x λx.x

{- (λx.x λy.y) (λz.z u) -> u

           @
       ⡀ ⠁   ⠈ ⢀
     @           @
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀
  λ     λ     λ     u
 ⡈ ⢁   ⡈ ⢁   ⡈ ⢁
x   x y   y z   z

--

           0
       ⡀ ⠁   ⠈ ⢀
    -2           2
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀
 -4     0     0     4
 ⡈ ⢁   ⡈ ⢁   ⡈ ⢁
x  -2 y   2 z   2
↑     ↑     ↑
-6    -2    -2
       

-- balance current row (symmetric around 0)
-- balance upward rows

--

           0
       ⡀ ⠁   ⠈ ⢀
    -3           3
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀
 -5    -1     1     5
 ⡈ ⢁   ⡈ ⢁   ⡈ ⢁
x  -3 y   1 z   3
↑     ↑     ↑
-7    -3    -1
       

-- [(-3,-3), (1,-1)]

--

           0
       ⡀ ⠁   ⠈ ⢀
    -6           6
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀
 -9    -3     3     9
 ⡈ ⢁   ⡈ ⢁   ⡈ ⢁
x  -7 y  -1 1   5
↑     ↑     
-11   -5   
       

--

           0
       ⡀ ⠁   ⠈ ⢀
    -6           6
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀
 -9    -3     3     9
 ⡈ ⢁   ⡈ ⢁   ⡈ ⢁
x  -7 y  -1 1   5
↑     ↑
-11   -5

-}
a5 :: Expr
a5 = (λ "x" "x" ∘ λ "y" "y") ∘ (λ "z" "z" ∘ "u")

a5' :: TExpr
a5' = (λ' 1 1 • λ' 2 2) • (λ' 3 3 • 0)

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

-- (λabc.cba)zz(λwv.w)
-- (λx.λy.xyy)(λa.a)b
-- (λy.y)(λx.xx)(λz.zq)
-- (λz.z)(λz.zz)(λz.zy)
-- (λx.λy.xyy)(λy.y)y
-- (λa.aa)(λb.ba)c
-- (λxyz.xz(yz))(λx.z)(λx.a)
-- λx.xxx z
