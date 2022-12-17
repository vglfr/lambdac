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
a3e = undefined

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

a4e :: Expr
a4e = undefined

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

-- (λabc.cba)zz(λwv.w)
-- (λx.λy.xyy)(λa.a)b
-- (λy.y)(λx.xx)(λz.zq)
-- (λz.z)(λz.zz)(λz.zy)
-- (λx.λy.xyy)(λy.y)y
-- (λa.aa)(λb.ba)c
-- (λxyz.xz(yz))(λx.z)(λx.a)
-- λx.xxx z
