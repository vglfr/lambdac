# Examples

Expression examples.

## Var:

#### `x -> x` (v1)

```
x
```

## Abs:

#### `λx.x -> λx.x` (f1)

```
  λ
 / \
x   x
```

#### `λxy.x -> λxy.x` (f2)

```
  λ
 / \
x   λ
   / \
  y   x
```

#### `λxyz.x -> λxyz.x` (f3)

```
  λ
 / \
x   λ
   / \
  y   λ
     / \
    z   x
```

#### `λxyz.xyz -> λxyz.xyz` (f4)

```
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
```


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

## App:

#### `x x -> x x` (a1)

```
  @
 / \
x   x
```

#### `x x x -> x x x` (a2)

```
    @
   / \
  @   x
 / \
x   x
```

#### `λx.x x -> x` (a3)

```
    @
   / \
  λ   x
 / \
x   x
```

-- x λx.x

-- λx.x λx.x

-- λx.x λy.y

-- (λz.zz)(λy.yy)

-- λx.x λx.x λx.x


-- (λx.x)(λy.y)z
-- ((λx.x)(λy.y))z
-- (λxy.xxy)(λx.xy)(λx.xz)
-- (λxyz.xz(yz))(λnn.n)(λp.p)
-- (λy.λz(λn.λn.n)z(yz))(λp.p)
-- λz(λn.z)((λp.p)z)

#### `λx.xxx z` (a4)

```
    @
   / \
  λ   z
 / \
x   @
   / \
  @   x
 / \
x   x
```

-- (λabc.cba)zz(λwv.w)
-- (λx.λy.xyy)(λa.a)b
-- (λy.y)(λx.xx)(λz.zq)
-- (λz.z)(λz.zz)(λz.zy)
-- (λx.λy.xyy)(λy.y)y
-- (λa.aa)(λb.ba)c
-- (λxyz.xz(yz))(λx.z)(λx.a)

## Old

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
