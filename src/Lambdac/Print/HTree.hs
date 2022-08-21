{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Lambdac.Print.HTree where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Debug.Trace
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
  deriving (Show, Foldable, Functor)

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
leaves _             = []

left :: PTree a -> a
left (PNode _ l _) = root l
left (PLeaf _)     = error "leaf has no left branch"

-- right :: PTree a -> a
-- right (PNode _ _ r) = root r
-- right (PLeaf _)     = error "leaf has no right branch"

data Elem = Elem { tag :: String, off :: Int, width :: Int } deriving Show

data Style = SPJ | Braille | HTree deriving (Show, Eq)

fill :: Style -> Int -> String
fill f n
  | n == 0       = ""
  | f == SPJ     = " / \\"
  | f == Braille = fromJust $ lookup n [(2, " ⡈ ⢁"), (3, "⡀⠁ ⠈⢀")] 
  | f == HTree   = let hh = replicate (n - 1) '─' 
                    in "┌" ++ hh ++ "┴" ++ hh ++ "┐"

class Tree a where
  tree :: a -> String

instance Tree Expr where
  -- tree = plot . offset . balance . build
  tree = plot . offset . build
   where
    build :: Expr -> PTree Elem
    build = go 0
     where
      go o e = case e of
                 Var v   -> PLeaf (Elem  v  o 0)
                 Abs h b -> PNode (Elem "λ" o 2) (go (o-2) h) (go (o+2) b)
                 App f x -> PNode (Elem "@" o 2) (go (o-2) f) (go (o+2) x)

    -- balance :: PTree Elem -> PTree Elem
    -- balance = go 0
     -- where
      -- go n t = if null $ row n t
                  -- then t
                  -- else let t' = modify n t
                        -- in if overlap n t'
                              -- then back n t'
                              -- else go (n+1) t'

      -- modify n t = let es = row n t
                    -- in undefined

      -- overlap n t = let os = off <$> row n t
                     -- in any (<2) $ zipWith (-) (tail os) os

      -- back 0 t = go 0 t
      -- back n t = let t' = undefined
                  -- in back (n-1) t'

    offset :: PTree Elem -> PTree Elem
    offset t = let d = negate $ minimum $ fmap off t
              in fmap (\(Elem v o w) -> Elem v (o + d) w) t

    plot :: PTree Elem -> String
    plot t = let s = style t
              in unlines $ go s 0
     where
      style t = let d = off (root t) - off (left t)
                 in if | d > 12 -> HTree
                       | d > 2  -> Braille
                       | otherwise -> SPJ

      go s n = let es = row n t
                in if null es
                      then []
                      else foldl' draw "" es : grey (foldl' (draw' s) "" es) : go s (n+1)

      draw    acc e = acc ++ replicate (off e - length acc) ' ' ++ tag e
      draw' s acc e = acc ++ replicate (off e - length acc - width e) ' ' ++ fill s (width e)

      grey s = "\ESC[30m" ++ s ++ "\ESC[m"
