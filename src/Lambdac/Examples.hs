{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Examples where

import Lambdac.Syntax
import Lambdac.Typechecker

{- λxyz.x -> λxyz.x

  λ
 / \
x   λ
   / \
  y   λ
     / \
    z   x
-}
f1 :: Expr
f1 = λ "x" (λ "y" (λ "z" "x"))

f1' :: TExpr
f1' = λ' "1" (λ' "2" (λ' "3" "1"))

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
f2 :: Expr
f2 = λ "x" (λ "y" (λ "z" ("x" ∘ "y" ∘ "z")))

-- f2' :: TExpr
-- f2' = λ' "1" (λ' "y" (λ' "z" ("x" • "y" • "z")))

-- λx.(λy.xy)
-- λx.xxx
-- λxy.xy
-- λxy.xz
-- λxy.zx
-- λxy.xxy
-- λxy.xy(zxy)
-- λxyz.zx
-- λxyz.xy(zx)
-- λxyz.xy(zxy)
-- λxyz.(λu.v)yz
-- λxyz.x(λu.v)z
-- λxyz.xy(λu.v)

{- x x -> x x

  @
 / \
x   x
-}
a1 :: Expr
a1 = "x" ∘ "x"

-- a2 :: TExpr
-- a2 = "0" • "0"

{- x x x -> x x x

    @
   / \
  @   x
 / \
x   x
-}
a2 :: Expr
a2 = "x" ∘ "x" ∘ "x"

{- λx.x x -> x

    @
   / \
  λ   x
 / \
x   x
-}
a3 :: Expr
a3 = λ "x" "x" ∘ "x"

-- x λx.x
-- λx.x λx.x
-- λx.x λy.y
-- (λz.zz)(λy.yy)
-- λx.x λx.x λx.x

{- (λx.x λy.y) (λz.z u) -> u

           @
     @          @
   /   \       / \
  λ     λ     λ   u
 / \   / \   / \
x   x y   y z   z
-}
a4 :: Expr
a4 = (λ "x" "x" ∘ λ "y" "y") ∘ (λ "z" "z" ∘ "u")

a4' :: TExpr
a4' = (λ' "1" "1" • λ' "1" "1") • (λ' "1" "1" • "0")

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
a5 :: Expr
a5 = λ "x" ("x" ∘ "x" ∘ "x") ∘ "z"

a5' :: TExpr
a5' = λ' "1" ("1" • "1" • "1") • "0"

-- (λabc.cba)zz(λwv.w)
-- (λx.λy.xyy)(λa.a)b
-- (λy.y)(λx.xx)(λz.zq)
-- (λz.z)(λz.zz)(λz.zy)
-- (λx.λy.xyy)(λy.y)y
-- (λa.aa)(λb.ba)c
-- (λxyz.xz(yz))(λx.z)(λx.a)

-- Old

-- (λx.x)(λy.y)
-- (λx.x)(λy.y)z
-- ((λx.x)(λy.y))z
-- λxy.xy
-- λx.(λy.xy)
-- (λxy.xxy)(λx.xy)(λx.xz)
-- (λxyz.xz(yz))(λnn.n)(λp.p)
-- (λy.λz(λn.λn.n)z(yz))(λp.p)
-- λz(λn.z)((λp.p)z)
-- λxy.xz
-- λxy.xxy
-- λxyz.zx
-- (λx.xxx)z
-- (λabc.cba)zz(λwv.w)
-- (λz.zz)(λy.yy)
-- λx.xxx
-- λxy.zx
-- λxyz.xy(zx)
-- λxyz.xy(zxy)
-- λxy.xy(zxy)
-- (λx.λy.xyy)(λa.a)b
-- (λy.y)(λx.xx)(λz.zq)
-- (λz.z)(λz.zz)(λz.zy)
-- (λx.λy.xyy)(λy.y)y
-- (λa.aa)(λb.ba)c
-- (λxyz.xz(yz))(λx.z)(λx.a)
