{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Print.HTree where

import Data.Foldable (foldl')
import Data.List (singleton)
import Data.Maybe (fromJust)
import Debug.Trace
import Lambdac.Helper.DTree (DTree (..), row)
import Lambdac.Print.Show ()
import Lambdac.Syntax (Expr (..), symbol)

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

data PElem = PElem { string :: String, offset :: Int } deriving Show

data Style = Braille | Pipe | None | SPJ deriving (Show, Eq)

tree' :: Expr -> DTree PElem
tree' e = go 0 Nothing e
 where
  go :: Int -> Maybe (DTree PElem) -> Expr -> DTree PElem
  go o p e = case e of
               Var v   -> DNode p (PElem v o) Nothing Nothing
               Abs h b -> fromNode "λ" o p h b
               App f x -> fromNode "@" o p f x
  fromNode c o p l r = let p' = DNode
                                  { top   = p
                                  , value = PElem c o
                                  , left  = Just $ go (o-2) (Just p') l
                                  , right = Just $ go (o+2) (Just p') r }
                        in p'

spread :: DTree PElem -> DTree PElem
spread t = let _ = spreadRow $ row 3 t
            in t

spreadRow :: [PElem] -> [PElem]
spreadRow = go []
 where
  go as bs = if length bs < 2
               then as <> bs
               else let [a, b] = take 2 bs
                        d      = distance a b
                     in if d < 2
                        then let (q, r) = divMod (2-d) 2
                                 ls = shift (-q) $ as <> singleton a
                                 rs = shift ( q+r) $ tail bs
                              in go ls rs
                        else go (as <> singleton a) (tail bs)
  distance a b = offset b - offset a
  shift n = map (\x -> x { offset = offset x + n })

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
  -- tree = plot . offsetLeft . balance . build
  -- tree = show . depth . build
  tree = plot . offsetLeft . build
   where
    build :: Expr -> DTree PElem
    build = go 0
     where
      go = undefined
      -- go o e = case e of
      --              Var v   -> PLeaf (PElem  v  o) Nothing
      --              Abs h b -> PNode (PElem "λ" o) Nothing (go (o-2) h) (go (o+2) b)
      --              App f x -> PNode (PElem "@" o) Nothing (go (o-2) f) (go (o+2) x)

    offsetLeft :: DTree PElem -> DTree PElem
    offsetLeft t = let d = negate $ minimum $ fmap offset t
                    in fmap (\(PElem v o) -> PElem v (o + d)) t

    plot :: DTree PElem -> String
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
                      else foldl' draw "" es : grey (foldl' (draw' s) "" es) : go s (n+1)

      draw    acc e = acc ++ replicate (offset e - length acc) ' ' ++ string e
      draw' s acc e = acc ++ replicate (offset e - length acc) ' ' ++ fill s 0

      grey s = "\ESC[30m" ++ s ++ "\ESC[m"
