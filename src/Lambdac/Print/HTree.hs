{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Print.HTree where

import Data.Foldable (foldl')
import Data.List (singleton)
import Data.Maybe (fromJust)
import Debug.Trace
import Lambdac.Helper.DTree (DTree (..), depth, parent, row)
import Lambdac.Print.Show ()
import Lambdac.Syntax (Expr (..), symbol)

{-

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

data PElem = PElem { string :: String, offset :: Int } deriving (Eq, Show)

data Style = Braille | Pipe | None | SPJ deriving (Show, Eq)

valueOffset :: DTree PElem -> Int
valueOffset = offset . value

valueString :: DTree PElem -> String
valueString = string . value

tree' :: Expr -> DTree PElem
tree' = go 0 Nothing
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
spread t = go (depth t) t
 where
  go 1 t = t
  go n t = go (n-1) (spreadRow n t)

spreadRow :: Int -> DTree PElem -> DTree PElem
spreadRow n t = case go n t of
                   (True,  t') -> t'
                   (False, t') -> go n t'
 where
  go n t = let r = row n t
            in undefined

spreadRow' :: [DTree PElem] -> [DTree PElem]
spreadRow' = go []
 where
  go :: [DTree PElem] -> [DTree PElem] -> [DTree PElem]
  go as bs = if length bs < 2
               then as <> bs
               else let [a, b] = take 2 bs
                        d      = distance a b
                     in if d < 2
                        then let -- p = parent a b
                                 (q, r) = divMod (2-d) 2
                                 ls = map (shiftBelow (-q)) $ as <> singleton a
                                 rs = map (shiftBelow ( q+r)) $ tail bs
                              in go ls rs
                        else go (as <> singleton a) (tail bs)
  distance a b = valueOffset b - valueOffset a

shiftBelow :: Int -> DTree PElem -> DTree PElem
shiftBelow n t = let v  = value t
                     v' = v { offset = offset v + n }
                     l' = shiftBelow n <$> left t
                     r' = shiftBelow n <$> right t
                  in DNode (top t) v' l' r'

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
  tree = plot . shiftRight . build
   where
    build :: Expr -> DTree PElem
    build = go 0 Nothing
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

    shiftRight :: DTree PElem -> DTree PElem
    shiftRight t = let d = negate $ minimum $ fmap offset t
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

      draw    acc e = acc ++ replicate (valueOffset e - length acc) ' ' ++ valueString e
      draw' s acc e = acc ++ replicate (valueOffset e - length acc) ' ' ++ fill s 0

      grey s = "\ESC[30m" ++ s ++ "\ESC[m"
