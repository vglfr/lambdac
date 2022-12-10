{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Print.HTree where

import Data.Foldable (foldl')
import Data.List (elemIndex, singleton)
import Data.List.Extra (trimEnd)
import Data.Maybe (fromJust)
import Debug.Trace
import Lambdac.Helper.DTree (DTree (..), depth, parent, root, row)
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

edges = [ (2, " / \\")
        , (3, " ⡀⠁ ⠈⢀")
        , (4, " ⡀⋅⠁ ⠈⢀")
        , (5, " ⡀⠁   ⠈⢀") ] 

data PElem = PElem { string :: String, offset :: Int } deriving (Eq, Show)

data Style = Pipe | SPJB deriving (Show, Eq)

htree :: Expr -> String
htree = plot SPJB . shiftRight . spread . build
 where
  shiftRight :: DTree PElem -> DTree PElem
  shiftRight t = let d = negate $ minimum $ fmap offset t
                  in fmap (\(PElem v o) -> PElem v (o + d)) t

  plot :: Style -> DTree PElem -> String
  plot s t = trimEnd . unlines $ go 0
   where
    go n = let es = row n t
            in if null es
                  then []
                  else foldl' drawValue "" es : grey (foldl' drawSpine "" es) : go (n+1)

    drawValue acc e = acc ++ replicate (valueOffset e - length acc) ' ' ++ valueString e
    drawSpine acc e = case left e of
                        Nothing -> acc
                        Just e' -> let d = distance e e'
                                    in acc ++ replicate (valueOffset e' - length acc) ' ' ++ fill s d

    fill :: Style -> Int -> String
    fill s n
      | n == 0    = ""
      | s == SPJB = fromJust $ lookup n edges
      | s == Pipe = let hh = replicate (n - 1) '─' 
                     in "┌" ++ hh ++ "┴" ++ hh ++ "┐"

    grey s = "\ESC[30m" ++ s ++ "\ESC[m"
  
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

spread :: DTree PElem -> DTree PElem
spread t = go (depth t) t
 where
  go 1 t = t
  go n t = go (n-1) (spreadRow n t)

spreadRow :: Int -> DTree PElem -> DTree PElem
spreadRow n t = case spreadOnce n t of
                   (True,  _ ) -> t
                   (False, t') -> spreadRow n t'
 where
  spreadOnce :: Int -> DTree PElem -> (Bool, DTree PElem)
  spreadOnce n t = case overlap $ row n t of
                     Nothing     -> (True, t)
                     Just (a, b) -> traceShowId $ (False, spreadParent a b)

spreadParent :: DTree PElem -> DTree PElem -> DTree PElem
spreadParent a b = let (q, r) = divMod (2 - distance a b) 2
                       p  = parent a b
                       l' = shiftBelow (-q)   <$> left p
                       r' = shiftBelow ( q+r) <$> right p
                    in root $ p { left = l', right = r' }

overlap :: [DTree PElem] -> Maybe (DTree PElem, DTree PElem)
overlap es = let os = zipWith (\a b -> distance b a < 2) es (tail es)
                 o  = elemIndex True os
              in (\i -> (es !! i, es !! (i+1))) <$> o

shiftBelow :: Int -> DTree PElem -> DTree PElem
shiftBelow n t = let v  = value t
                     v' = v { offset = offset v + n }
                     l' = shiftBelow n <$> left t
                     r' = shiftBelow n <$> right t
                  in DNode (top t) v' l' r'

valueOffset :: DTree PElem -> Int
valueOffset = offset . value

valueString :: DTree PElem -> String
valueString = string . value

distance :: DTree PElem -> DTree PElem -> Int
distance a b = valueOffset a - valueOffset b

-- guessStyle t = let d = off (root t) - off (left t)
--            in if | d > 12 -> Pipe
--                  | d > 2  -> Braille
--                  | otherwise -> SPJ
