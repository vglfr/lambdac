module Lambdac.Example.Var where

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
