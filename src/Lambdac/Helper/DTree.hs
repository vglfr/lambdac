{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Lambdac.Helper.DTree where

data DTree a
  = DRoot {                 value :: a, left :: DTree a, right :: DTree a }
  | DNode { top :: DTree a, value :: a, left :: DTree a, right :: DTree a }
  | DLeaf { top :: DTree a, value :: a }
  | DSimp {                 value :: a }
  deriving (Foldable, Functor)
 
instance Show a => Show (DTree a) where
  show (DRoot   v l r) = "(DRoot ("                   ++ show v ++ ") " ++ show l ++ " " ++ show r ++ ")"
  show (DNode _ v l r) = "(DNode ([parent:cyclic]) (" ++ show v ++ ") " ++ show l ++ " " ++ show r ++ ")"
  show (DLeaf _ v)     = "(DLeaf ([parent:cyclic]) (" ++ show v ++ "))"
  show (DSimp   v)     = "(DSimp ("                   ++ show v ++ "))"

depth :: DTree a -> Int
depth = go 0
 where
  go d t = case t of
             DSimp _       -> 1
             DLeaf _ _     -> d
             DNode _ _ l r -> max (go (d+1) l) (go (d+1) r)

row :: Int -> DTree a -> [a]
row n t = go n [t]
 where
  go 0 ts = map value ts
  go n ts = go (n-1) (concatMap leaves ts)

leaves :: DTree a -> [DTree a]
leaves (DRoot   _ l r) = [l, r]
leaves (DNode _ _ l r) = [l, r]
leaves (DLeaf _ _)     = []
leaves (DSimp _)       = []

-- get bitmask from index
-- traverse to the element in question
-- substitute it
substitute :: a -> Int -> DTree a -> DTree a
substitute e i t = undefined
