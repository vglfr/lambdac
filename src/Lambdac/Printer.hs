module Lambdac.Printer where

import Lambdac.Syntax

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

λx.xx λy.yy

     ∙
    / \
   /   \
  λ     λ
 / \   / \
x   ∙ y   ∙
   / \   / \
  x   x y   y

λx.x(λz.z) λy.(λz.z)y

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
  [∙]
  [λ, λ]
  [x, ∙, y, ∙]
  [x, λ, λ, y]
  [z, z, z, z]
]

1-offset
[
  [0]
  [-1, 1]
  [-2, 0, 0, 2]
  [-1, 1, 1, 3]
  [ 0, 2, 0, 2]
]

offset * 2
[
  [0]
  [-2, 2]
  [-4, 0, 0, 4]
  [-2, 2, 2, 6]
  [ 0, 4, 0, 4]
]

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

Bad:

       ∙
      / \
     /   λ
    /   / \
   /   y   ∙
  λ       / \
 / \     λ   y
x   ∙   / \
   / \ z   z
  x   λ
     / \
    z   z

       ∙
  λ         λ
 / \       / \
x   ∙     y   ∙
   / \       / \
  x   λ     λ   y
     / \   / \
    z   z z   z

     ∙
  λ     λ
 / \   / \
x   ∙ y   ∙
   / \   / \
  x   x y   y

      ∙
     / \
    /   \
   /     \
  λ       λ
 / \     / \
x   ∙   y   ∙
   / \     / \
  x   x   y   y

   ∙
 λ    λ
x ∙  y ∙
 x x  y y

 -}

type Elements   = [[String]]
type Offsets    = [[Int]]
type RowOffsets = [Int]

elements :: Elements
elements =
  [ ["∙"]
  , ["λ", "λ"]
  , ["x", "∙", "y", "∙"]
  , ["x", "λ", "λ", "y"]
  , ["z", "z", "z", "z"]
  ]

offsets :: Offsets
offsets =
  -- [ [0]
  -- , [-1, 1]
  -- , [-2, 0, 0, 2]
  -- , [-1, 1, 1, 3]
  -- , [ 0, 2, 0, 2]
  -- ]
  [ [3]
  , [-2, 8]
  , [-4, 0, 6, 10]
  , [-2, 2, 8, 12]
  , [ 0, 4, 6, 10]
  ]

rowOffsets :: [Int]
rowOffsets = [5, 2, 2, 2, 0]

draw :: Elements -> Offsets -> RowOffsets -> IO ()
draw es os rs = mapM_ f (zip3 es os rs)
 where
  f (es',os',r) = do
    let o  = head os'
        ds = map (\(e,s) -> e - s - 1) $ zip (tail os') os'
        s  = replicate (5 + head os') ' ' ++ head es' ++ concatMap offset (zip (tail es') ds)
        s' = rowOffset r
    putStrLn s
    putStrLn s'
  offset (e,o) = replicate o ' ' ++ e
  rowOffset n = case n of
                  5 -> "  / \\\n /   \\\n/     \\"
                  2 -> "/ \\"
                  _ -> ""
