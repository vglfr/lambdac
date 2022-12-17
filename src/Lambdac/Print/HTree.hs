{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Print.HTree where

import Data.Foldable (foldl')
import Data.List (elemIndex, singleton)
import Data.List.Extra (trimEnd)
import Data.Maybe (fromJust)
import Debug.Trace
import Lambdac.Helper.DTree (DTree (..), depth, parent, relink, row)
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

class ToString a where
  toString :: a -> String
 
instance ToString String where
  toString = id
 
instance ToString Int where
  toString = show

htree :: Expr -> String
htree = plot SPJB string . shiftRight . build

shiftRight :: DTree PElem -> DTree PElem
shiftRight t = let d = negate $ minimum $ fmap offset t
                in fmap (\(PElem v o) -> PElem v (o + d)) t
        
plot :: ToString a => Style -> (PElem -> a) -> DTree PElem -> String
plot s f t = trimEnd . unlines $ go 0
 where
  go n = let es = row n t
          in if null es
                then []
                else foldl' drawValue "" es : grey (foldl' drawSpine "" es) : go (n+1)

  drawValue acc e = acc ++ replicate (valueOffset e - length acc) ' ' ++ (toString . f . value $ e)
  drawSpine acc e = case left e of
                      Nothing -> acc
                      Just e' -> let d = distance e' e
                                  in acc ++ replicate (valueOffset e' - length acc) ' ' ++ fill s d

  fill :: Style -> Int -> String
  fill s n
    | n == 0    = ""
    | s == SPJB = fromJust $ lookup n edges
    | s == Pipe = let hh = replicate (n-1) '─' 
                   in "┌" ++ hh ++ "┴" ++ hh ++ "┐"

  grey s = if null s
           then s
           else "\ESC[30m" ++ s ++ "\ESC[m"
  
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
                     Nothing    -> (True, t)
                     Just (a,b) -> (False, spreadParent a b)

spreadParent :: DTree PElem -> DTree PElem -> DTree PElem
spreadParent a b = let (q,r) = quotRem (2 - distance a b) 2
                       p  = parent a b
                       l' = shiftBelow (-q)   p' <$> left p
                       r' = shiftBelow ( q+r) p' <$> right p
                       p' = p { left = l', right = r' }
                    in relink p p'
          
overlap :: [DTree PElem] -> Maybe (DTree PElem, DTree PElem)
overlap es = let os = zipWith (\a b -> distance a b < 2) es (tail es)
                 o  = elemIndex True os
              in (\i -> (es !! i, es !! (i+1))) <$> o

shiftBelow :: Int -> DTree PElem -> DTree PElem -> DTree PElem
shiftBelow n p t = let v  = value t
                       v' = v { offset = offset v + n }
                       l' = shiftBelow n t' <$> left t
                       r' = shiftBelow n t' <$> right t
                       t' = DNode (Just p) v' l' r' 
                    in t'

valueOffset :: DTree PElem -> Int
valueOffset = offset . value

valueString :: DTree PElem -> String
valueString = string . value

distance :: DTree PElem -> DTree PElem -> Int
distance a b = valueOffset b - valueOffset a

-- guessStyle t = let d = off (root t) - off (left t)
--            in if | d > 12 -> Pipe
--                  | d > 2  -> Braille
--                  | otherwise -> SPJ
