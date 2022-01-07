{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFoldable #-}

module Lambdac.Printer where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Lambdac.Print.Show
import Lambdac.Syntax

{-

           depth
             ↓
-- treeN :: Int -> Tree -> String
-- tree5 ::        Tree -> String
       ↑
      TH?

                LRRRLRL-like path to start
                            ↓
-- treeP :: Int -> Tree -> Path -> String

                       @
              ⡀ ⠄ ⠂ ⠁     ⠈ ⠐ ⠠ ⢀                -- 12
           @                       @
       ⡀ ⠁   ⠈ ⢀               ⡀ ⠁   ⠈ ⢀         -- 6
     @           @           @           @
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀     -- 3
  @     @     @     @     @     @     @     @
 / \   / \   / \   / \   / \   / \   / \   / \   -- 2
x   x y   y z   z u   u v   v w   w i   i j   j

  x    x     x      x
 /   ⡀⠁   ⡀⋅⠁   ⡀⠄⠂⠁
x   x    x     x

  x    x       x          x               x
 ⡈   ⡀⠁    ⡀⋅⠁      ⡀⠄⠂⠁        ⡀ ⠄ ⠂ ⠁
x   x    x       x          x

-}

data PTree a
  = PNode a (PTree a) (PTree a)
  | PLeaf a
  deriving (Show, Foldable)

row :: Int -> PTree a -> [a]
row n t = go n [t]
 where
  go 0 ts = map root ts
  go n ts = go (n-1) (concatMap leaves ts)

root :: PTree a -> a
root (PNode v _ _) = v
root (PLeaf v)     = v

leaves :: PTree a -> [PTree a]
leaves (PNode _ l r) = [l, r]
leaves n             = [n]

left :: PTree a -> a
left (PNode _ l _) = root l
left (PLeaf _)     = error "leaf has no left branch"

-- right :: PTree a -> a
-- right (PNode _ _ r) = root r
-- right (PLeaf _)     = error "leaf has no right branch"

data Elem = Elem { tag :: String, off :: Int, width :: Int }

data Style = SPJ | Braille | HTree deriving Eq

fill :: Style -> Int -> String
fill f n
  | n == 0       = "  "
  | f == SPJ     = "/ \\"
  | f == Braille = fromJust $ lookup n [(2, "⡈ ⢁"), (3, "⡀⠁ ⠈⢀")] 
  | f == HTree   = let hh = replicate (n - 2) '─' 
                    in "┌" ++ hh ++ "┴" ++ hh ++ "┐"

class Tree a where
  tree :: a -> String

instance Tree Expr where
  tree = plot . balance . build
   where
    build :: Expr -> PTree Elem
    build e = case e of
      Var v   -> PLeaf (Elem  v  0 0)
      Abs h b -> PNode (Elem "λ" 0 0) (build h) (build b)
      App f x -> PNode (Elem "@" 0 0) (build f) (build x)

    balance :: PTree Elem -> PTree Elem
    balance = go 0
     where
      go n t = if null $ row n t
                  then t
                  else let t' = modify n t
                        in if overlap n t'
                              then back n t'
                              else go (n+1) t'

      modify n t = let es = row n t
                    in undefined

               -- case t of
                 -- PLeaf e     -> PLeaf $ e { off = n }
                 -- PNode e l r -> undefined

      overlap n t = let os = off <$> row n t
                     in any (<2) $ zipWith (-) (tail os) os

      back 0 t = go 0 t
      back n t = let t' = undefined
                  in back (n-1) t'

    plot :: PTree Elem -> String
    plot t = let s = style t
                 o = offset t
              in unlines $ go s o 0
     where
      style t = let d = diff t
                 in if | d > 15 -> HTree
                       | d > 1  -> Braille
                       | otherwise -> SPJ

      diff t = off (root t) - off (left t)
      offset = negate . foldr (\x acc -> min acc (off x)) 0

      go s o n = let es = row n t
                  in if null es
                        then []                   -- "\ESC[30m" ++ ++ "\ESC[m"
                        else foldl' (draw o) "" es : foldl' (draw' s o) "" es : go s o (n+1)

      draw    o acc e = acc ++ replicate (o - off e - length acc - 1) ' ' ++ tag e
      draw' s o acc e = acc ++ replicate (o - off e - length acc - width e - 1) ' ' ++ fill s (width e)
