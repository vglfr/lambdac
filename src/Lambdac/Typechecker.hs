module Lambdac.Typechecker where

import Data.String (IsString (..))

import Lambdac.Syntax

data TExpr
  = TVar Int
  | TAbs TExpr TExpr
  | TApp TExpr TExpr
  deriving (Show, Eq)

λ' :: TExpr -> TExpr -> TExpr
λ' = TAbs

(•) :: TExpr -> TExpr -> TExpr
(•) = TApp

instance IsString TExpr where
  fromString = TVar . read

-- reuse pprinter

ttree :: Expr -> TExpr
ttree = go []
 where
  go s e = case e of
    App f x -> let s' = e:s in TApp (go s' f) (go s' x)
    Abs h b -> let s' = e:s in TAbs (go s' h) (go s' b)
    Var _   -> TVar $ up s 1 e
  up []     _ _ = 0  -- add to bound dict
  up (e:es) n v = case e of
                    Var _   -> error "absurd"
                    App _ _ -> up es n v
                    Abs h _ -> if h == v then n else up es (n+1) v

-- arity :: TExpr
-- arity e = case e of
  -- Abs _ b -> 1 + arity b
  -- _       -> 0

tcheck :: TExpr -> TExpr
tcheck t = let t' = step t
               in if t == t'
                     then t
                     else tcheck t'

step :: TExpr -> TExpr
step t = case t of
  TVar n   -> TVar n
  TAbs h b -> TAbs h (step b)
  TApp f v -> case f of
                TVar n   -> TApp f (step v)
                TAbs h b -> fmap' (\x -> if x == h then v else x) b
                TApp _ _ -> TApp (step f) v
 where
  fmap' :: (TExpr -> TExpr) -> TExpr -> TExpr
  fmap' f v@(TVar _) = f v
  fmap' f (TAbs h b) = TAbs h (fmap' f b)
  fmap' f (TApp g x) = TApp (fmap' f g) (fmap' f x)
