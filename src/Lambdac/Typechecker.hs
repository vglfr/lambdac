module Lambdac.Typechecker where

import Data.String (IsString (..))

import Lambdac.Syntax

data TExpr
  = TVar Int
  | TAbs TExpr TExpr
  | TApp TExpr TExpr
  deriving (Show, Eq)

instance IsString TExpr where
  fromString = TVar . read

λ' :: TExpr -> TExpr -> TExpr
λ' = TAbs

(•) :: TExpr -> TExpr -> TExpr
(•) = TApp

-- reuse pprinter

ttree :: Expr -> TExpr
-- ttree = reverse . dedup . debrujin
ttree = dedup . debrujin
 where
  debrujin :: Expr -> TExpr
  debrujin = go []
   where
    go s e = case e of
      App f x -> let s' = e:s in TApp (go s' f) (go s' x)
      Abs h b -> let s' = e:s in TAbs (TVar 0)  (go s' b)
      Var _   -> TVar $ lookup s 1 e

    lookup []     _ _ = 0
    lookup (e:es) n v = case e of
                          Var _   -> error "absurd"
                          App _ _ -> lookup es n v
                          Abs h _ -> if h == v then n else lookup es (n+1) v

  dedup :: TExpr -> TExpr
  dedup t = down (-1) [] t
   where
    down :: Int -> [TExpr] -> TExpr -> TExpr
    down n us t = case t of
      TVar _   -> up n us t t
      TAbs _ b -> let b' = rename 1 n b
                   in down (n-1) (TAbs (TVar n) b' : us) b'
      TApp f _ -> down n (t:us) f

    up _ []     t _ = t
    up n (u:us) t d = case t of
      -- TVar _   -> u
      TVar _   -> up n us u t
      -- TAbs _ _ -> up n us u t -- (sub t u)
      -- TAbs _ _ -> u
      -- TAbs _ _ -> sub t u
      TAbs _ _ -> up n us (sub t u) t
      TApp f x -> if left d t
                     then down n (u:us) x
                     -- else up n us (sub t u) t
                     else up n us u t -- (sub t u)

    left t u = case u of
      TVar _   -> False
      TAbs h _ -> h == t
      TApp f _ -> f == t

    rename :: Int -> Int -> TExpr -> TExpr
    rename i n t = case t of
      TVar v   -> if v == i then TVar n else TVar v
      TAbs h b -> TAbs h (rename (i+1) n b)
      TApp f x -> TApp (rename i n f) (rename i n x)

    sub :: TExpr -> TExpr -> TExpr
    sub t u = let l = left t u
               in case u of
                    TVar _   -> error "absurd"
                    TAbs h b -> if l then TAbs t b else TAbs h t
                    TApp f x -> if l then TApp t x else TApp f t

  reverse :: TExpr -> TExpr
  reverse t = case t of
    TVar v   -> TVar (negate v)
    TAbs h b -> TAbs (reverse h) (reverse b)
    TApp f x -> TApp (reverse f) (reverse x)

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
                TAbs h b -> sub h v b
                TApp _ _ -> TApp (step f) v
 where
  sub h v b = case b of
    TVar _   -> if b == h then v else b
    TAbs h b -> TAbs h (sub h v b)
    TApp g x -> TApp (sub h v g) (sub h v x)
