{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Lambdac.Helper.DTree where

import Data.Maybe (catMaybes)
import GHC.Show (showSpace)

data DTree a = DNode
  { top   :: Maybe (DTree a)
  , value :: a
  , left  :: Maybe (DTree a)
  , right :: Maybe (DTree a) }
  deriving (Foldable, Functor)
 
instance Show a => Show (DTree a) where
  showsPrec _ (DNode t v l r) = showParen True showAll
   where
    showAll   = showName . showSpace . showTop . showSpace . showValue . showSpace . showLeft . showSpace . showRight
    showName  = showString "DNode"
    showTop   = showString $ maybe "[none]" (const "[cyclic]") t
    showValue = showParen True $ shows v
    showLeft  = showString $ maybe "[none]" show l
    showRight = showString $ maybe "[none]" show r

depth :: DTree a -> Int
depth = go 0 . Just
 where
  go d t = let d' = d+1
            in case t of
                 Nothing -> d'
                 Just t' -> max (go d' (left t')) (go d' (right t'))

row :: Int -> DTree a -> [a]
row n t = go n [t]
 where
  go 0 ts = map value ts
  go n ts = go (n-1) (concatMap leaves ts)

leaves :: DTree a -> [DTree a]
leaves t = catMaybes [left t, right t]

root :: DTree a -> DTree a
root t = maybe t root (top t)

-- get bitmask from index
-- traverse to the element in question
-- substitute it
substitute :: a -> Int -> DTree a -> DTree a
substitute e i t = undefined
