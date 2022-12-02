module Lambdac.Typechecker where

import Lambdac.Syntax (Expr (..), TExpr (..))


get :: String -> TExpr -> TExpr
get ps t = go (init ps) t
 where
   go "" t = t
   go ps t = let left = last ps == 'L'
                 ps'  = init ps
              in case t of
                   TVar _   -> t
                   TAbs h b -> if left then go ps' h else go ps' b
                   TApp f x -> if left then go ps' f else go ps' x

set :: TExpr -> String -> TExpr -> TExpr
set v ps t = go (init ps) t
 where
   go "" t = v
   go ps t = let left = last ps == 'L'
                 ps'  = init ps
              in case t of
                   TVar _   -> v
                   TAbs h b -> if left then TAbs (go ps' h) b else TAbs h (go ps' b)
                   TApp f x -> if left then TApp (go ps' f) x else TApp f (go ps' x)

ttree :: Expr -> TExpr
ttree = negate . dedup . debrujin
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
  dedup t = down (-1) "T" t
   where
    down n ps t = case get ps t of
                    TVar _   -> up n (tail ps) (head ps) t
                    TAbs h b -> let b' = rename 1 n b
                                    t' = set (TAbs (TVar n) b') ps t
                                 in down (n-1) ('R' : ps) t'
                    TApp _ _ -> down n ('L' : ps) t

    up _ _  'T' t = t
    up n ps  p  t = case get ps t of
                      TVar _   -> up n (tail ps) (head ps) t
                      TAbs _ _ -> up n (tail ps) (head ps) t
                      TApp f x -> if p == 'L'
                                     then down n ('R' : ps) t
                                     else up n (tail ps) (head ps) t

    rename i n t = case t of
                     TVar v   -> if v == i then TVar n else TVar v
                     TAbs h b -> TAbs h (rename (i+1) n b)
                     TApp f x -> TApp (rename i n f) (rename i n x)

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
