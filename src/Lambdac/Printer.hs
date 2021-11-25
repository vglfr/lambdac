{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Printer where

import Lambdac.Syntax

import Control.Monad (join)

instance Show Expr where
  show (Var x)   = x
  show (Abs h b) = "λ" ++ show h ++ showAbs b
   where
    showAbs (Abs h' b') = show h' ++ showAbs b'
    showAbs e           = "." ++ showInner e
    showInner e@(Abs _ _) = "(" ++ show e ++ ")"
    showInner (App f x)   = showInner f ++ showInner x
    showInner e           = show e
  show (App f x) = show f ++ " " ++ show x

class Repr a where
  repr :: a -> String
  tree :: a -> String

instance Repr Expr where
  repr (Var x)   = "(Var \"" ++ x ++ "\")"
  repr (Abs h b) = "(Abs " ++ repr h ++ " " ++ repr b ++ ")"
  repr (App f x) = "(App " ++ repr f ++ " " ++ repr x ++ ")"
  tree (Var x)   = x
  tree (Abs h b) = undefined

{-

λx.x(λz.z) λy.(λz.z)y

∙
├ λ
│ ├ x
│ └ ∙
│   ├ x
│   └ λ z z
└ λ
  ├ y
  └ ∙
    ├ λ z z
    └ y

Bad:

       ∙
      / \
     /   \
    /     \
   /       \
  λ         λ
 / \       / \
x   ∙     y   ∙
   / \       / \
  x   λ     λ   y
     / \   / \
    z   z z   z

[
  [0]
  [-2, 2]
  [-4, 0, 0, 4]
  [-2, 2, 2, 6]
  [ 0, 4, 6, 10] +6 for the RIGHTMOST branch
]

[
  [3]
  [-2, 8]
  [-4, 0, 6, 10]
  [-2, 2, 8, 12]
  [ 0, 4, 6, 10]
]

  ┌ x
┌ λ 
│ │ ┌ x
│ └ ∙
│   └ λ z z
∙ 
│ ┌ y
└ λ
  │ ┌ λ z z
  └ ∙
    └ y

∙
├ λ
│ ├ x
│ └ ∙
│   ├ x
│   └ λ
│     ├ z
│     └ z
└ λ
  ├ y
  └ ∙
    ├ λ
    │ ├ z
    │ └ z
    └ y

∙
├─ λ
│  ├─ x
│  └─ ∙
│     ├─ x
│     └─ λ
│        ├─ z
│        └─ z
└─ λ
   ├─ y
   └─ ∙
      ├─ λ
      │  ├─ z
      │  └─ z
      └─ y

  ┌ x
┌ λ 
│ │ ┌ x
│ └ ∙
│   │ ┌ z
│   └ λ
∙     └ z
│ ┌ y
└ λ   ┌ z
  │ ┌ λ
  │ │ └ z
  └ ∙
    └ y

 -}

data Slash = S Int | B Int

instance Show Slash where
  show (S _) = "\ESC[30m/\ESC[m"
  show (B _) = "\ESC[30m\\\ESC[m"

type Elements   = [[String]]
type Offsets    = [[Int]]
type Slashes    = [[Slash]]

root :: Expr -> String
root (Var x)   = x
root (Abs _ _) = "λ"
root (App _ _) = "⋅"

leaves :: Expr -> [Expr]
leaves (Var x)   = []
leaves (Abs h b) = [h, b]
leaves (App f x) = [f, x]

expr' :: Expr
-- expr' = λ "x" (λ "y" "x")
-- expr' = λ "x" "x"
expr' = λ "x" ("x" ⋅ λ "z" "z") ⋅ λ "y" (λ "z" "z" ⋅ "y")

elements :: Expr -> Elements
elements e = go [] [e]
 where
  go acc [] = acc
  go acc es = go (acc ++ [map root es]) (concatMap leaves es)

offsets :: Expr -> Offsets
offsets e = go [] [(e,0)]
 where
  go acc [] = acc
  go acc es = go (acc ++ [map snd es]) (concatMap leaves' es)

  leaves' (Var _, n) = []
  leaves' (    e, n) = let ls = leaves e
                        in [(head ls, n-2), (last ls, n+2)]

balance :: Offsets -> Offsets
balance os = let ls = last os
                 ds = diff ls
              in os

diff :: [Int] -> [Int]
diff xs = 0 : map (\(e,s) -> e - s) (zip (tail xs) xs)

-- slashes :: Expr -> Slashes
-- slashes = undefined

elements' :: Elements
elements' =
  [ ["∙"]
  , ["λ", "λ"]
  , ["x", "∙", "y", "∙"]
  , ["x", "λ", "λ", "y"]
  , ["z", "z", "z", "z"]
  ]

offsets' :: Offsets
offsets' =
  [ [3]
  , [-2, 8]
  , [-4, 0, 6, 10]
  , [-2, 2, 8, 12]
  , [ 0, 4, 6, 10]
  ]

slashes' :: Slashes
slashes' =
  [ [S 0]
  , [S (-3), B (-1), S 7, B 9]
  , [S (-1), B   1,  S 9, B 11]
  , [S   1,  B   3,  S 7, B 9]
  , [S 0]
  ]

draw :: Elements -> Offsets -> Slashes -> IO ()
draw es os ss = mapM_ f (zip3 es os ss)
 where
  f (es',os',ss') = do
    let o  = head os'
        ds = map (\(e,s) -> e - s - 1) $ zip (tail os') os'
        s  = replicate (5 + head os') ' ' ++ head es' ++ concatMap offset (zip (tail es') ds)
        so  = map (\s -> case s of (S n) -> n; (B n) -> n) ss'
        ds' = map (\(e,s) -> e - s - 1) $ zip (tail so) so
        s'  = replicate (5 + head so) ' ' ++ show (head ss') ++ concatMap offsetS (zip (tail ss') ds')
    putStrLn s
    putStrLn s'
  offset  (e,o) = replicate o ' ' ++ e
  offsetS (s,o) = replicate o ' ' ++ show s
