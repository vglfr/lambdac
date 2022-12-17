{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Example.Var where

import Lambdac.Syntax (Expr, TExpr)

-- x -> x

v1 :: Expr
v1 = "x"

v1' :: TExpr
v1' = 0

v1p :: String
v1p = "x"

v1r :: String
v1r = "(Var \"x\")"

v1v :: String
v1v = "x"

v1h :: String
v1h = "x"

-- x' -> x

v2 :: Expr
v2 = "x'"

v2' :: TExpr
v2' = 0

v2p :: String
v2p = "x'"

v2r :: String
v2r = "(Var \"x'\")"

v2v :: String
v2v = "x'"

v2h :: String
v2h = "x'"

-- x'' -> x

v3 :: Expr
v3 = "x''"

v3' :: TExpr
v3' = 0

v3p :: String
v3p = "x''"

v3r :: String
v3r = "(Var \"x''\")"

v3v :: String
v3v = "x''"

v3h :: String
v3h = "x''"

-- Old

-- λx.x λy.y
-- λx.x λy.y z
-- ((λx.x)(λy.y))z
-- λx.(λy.xy)
-- (λxy.xxy)(λx.xy)(λx.xz)
-- (λxyz.xz(yz))(λnn.n)(λp.p)
-- (λy.λz(λn.λn.n)z(yz))(λp.p)
-- λz(λn.z)((λp.p)z)
-- (λabc.cba)zz(λwv.w)
-- (λz.zz)(λy.yy)
-- λxyz.xy(zx)
-- λxyz.xy(zxy)
-- λxy.xy(zxy)
-- (λx.λy.xyy)(λa.a)b
-- (λy.y)(λx.xx)(λz.zq)
-- (λz.z)(λz.zz)(λz.zy)
-- (λx.λy.xyy)(λy.y)y
-- λa.aa λb.ba c
-- (λxyz.xz(yz))(λx.z)(λx.a)
