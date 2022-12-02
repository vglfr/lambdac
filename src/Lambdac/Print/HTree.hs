{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Lambdac.Print.HTree where

import Data.Foldable (foldl')
import Data.List (singleton)
import Data.Maybe (fromJust)
import Debug.Trace
import Lambdac.Print.Show ()
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
  deriving (Show, Foldable, Functor)

row :: Int -> PTree a -> [a]
row n t = go n [t]
 where
  go 0 ts = map root ts
  go n ts = go (n-1) (concatMap leaves ts)

depth :: PTree a -> Int
depth = go 0
 where
  go d t = case t of
             PLeaf _     -> d
             PNode _ l r -> max (go (d+1) l) (go (d+1) r)

root :: PTree a -> a
root (PNode v _ _) = v
root (PLeaf v)     = v

leaves :: PTree a -> [PTree a]
leaves (PNode _ l r) = [l, r]
leaves (PLeaf _)     = []

tree' :: Expr -> PTree Elem
tree' =  go 0
 where
  go o e = case e of
             Var v   -> PLeaf (Elem  v  o 0)
             Abs h b -> PNode (Elem "λ" o 2) (go (o-2) h) (go (o+2) b)
             App f x -> PNode (Elem "@" o 2) (go (o-2) f) (go (o+2) x)

spread :: [Elem] -> [Elem]
spread = go []
 where
  go as bs = if length bs < 2
               then as <> bs
               else let [a, b] = take 2 bs
                        d      = distance a b
                     in if d < 2
                        then let (q, r) = divMod (2 - d) 2
                                 ls = shift (-q) $ as <> singleton a
                                 rs = shift ( q + r) $ tail bs
                              in go ls rs
                        else go (as <> singleton a) (tail bs)
  distance a b = off b - off a
  shift n = map (\x -> x { off = off x + n })

-- left :: PTree a -> a
-- left (PNode _ l _) = root l
-- left (PLeaf _)     = error "leaf has no left branch"

-- right :: PTree a -> a
-- right (PNode _ _ r) = root r
-- right (PLeaf _)     = error "leaf has no right branch"

data Elem = Elem { tag :: String, off :: Int, width :: Int } deriving Show

data Style = Braille | Pipe | None | SPJ deriving (Show, Eq)

fill :: Style -> Int -> String
fill s n
  | n == 0       = ""
  | s == None    = ""
  | s == SPJ     = " / \\"
  | s == Braille = fromJust $ lookup n [(2, " ⡈ ⢁"), (3, "⡀⠁ ⠈⢀")] 
  | s == Pipe    = let hh = replicate (n - 1) '─' 
                    in "┌" ++ hh ++ "┴" ++ hh ++ "┐"

class Tree a where
  tree :: a -> String

instance Tree Expr where
  -- tree = plot . offset . balance . build
  -- tree = show . depth . build
  tree = plot . offset . build
   where
    build :: Expr -> PTree Elem
    build = go 0
     where
      go o e = case e of
                 Var v   -> PLeaf (Elem  v  o 0)
                 Abs h b -> PNode (Elem "λ" o 2) (go (o-2) h) (go (o+2) b)
                 App f x -> PNode (Elem "@" o 2) (go (o-2) f) (go (o+2) x)

    offset :: PTree Elem -> PTree Elem
    offset t = let d = negate $ minimum $ fmap off t
                in fmap (\(Elem v o w) -> Elem v (o + d) w) t

    plot :: PTree Elem -> String
    plot t = let s = style t
              in unlines $ go s 0
     where
      style _ = None
      -- style t = let d = off (root t) - off (left t)
      --            in if | d > 12 -> Pipe
      --                  | d > 2  -> Braille
      --                  | otherwise -> SPJ

      go s n = let es = row n t
                in if null es
                      then []
                      else foldl' draw "" es : go s (n+1)
                      -- else foldl' draw "" es : grey (foldl' (draw' s) "" es) : go s (n+1)

      draw    acc e = acc ++ replicate (off e - length acc) ' ' ++ tag e
      -- draw' s acc e = acc ++ replicate (off e - length acc - width e) ' ' ++ fill s (width e)

      grey s = "\ESC[30m" ++ s ++ "\ESC[m"
