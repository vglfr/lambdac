module Lambdac.Interpreter where

import Lambdac.Printer
import Lambdac.Syntax

import Data.List ((\\), nub, singleton)

-- 2 types of evaluation
-- - lazy  - do not recurse into functions: λx.(λy.y)z  ->  λx.(λy.y)z
-- - eager - do     recurse into functions: λx.(λy.y)z  ->  λx.z

-- data RType = Alpha | Beta

-- step :: Expr -> Maybe (Expr, RType, Expr, Expr)
-- step = undefined

-- try alpha
-- if successful then Just alpha result
-- otherwise try beta
-- if successful then Just beta result
-- otherwise check 
-- if alpha equivalent then Nothing

eval :: Expr -> Expr
eval e = do
  case alpha e of
    Just e' -> eval e'
    Nothing -> case beta e of
                 Just e' -> if isAlpha e e'
                            then e
                            else eval e'
                 Nothing -> e

evalPrint :: Expr -> IO ()
evalPrint e = do
  putStr "-> " >> print e
  case alpha e of
    Just e' -> evalPrint e'
    Nothing -> case beta e of
                 Just e' -> if isAlpha e e'
                            then putStrLn "ω" >> putStrLn ""
                            else evalPrint e'
                 Nothing -> putStrLn ""

--                        (e'  , f   , t   )
-- alpha :: Expr -> Maybe (Expr, Expr, Expr)
alpha :: Expr -> Maybe Expr
alpha (App f x) = let f' = foldr rename f (free x)
                  in if f == f' then Nothing else Just (App f' x)
alpha _ = Nothing

--                       (e'  , f   , t   )
-- beta :: Expr -> Maybe (Expr, Expr, Expr)
beta :: Expr -> Maybe Expr
beta (Var _)   = Nothing
beta (Abs h b) = case b of
  e@(App _ _) -> beta e >>= Just . Abs h
  _           -> Nothing
beta (App f x) = case f of
  Var _       -> Nothing
  Abs h b     -> Just $ replace h x b
  e@(App _ _) -> beta e >>= (\y -> Just $ App y x)

isAlpha :: Expr -> Expr -> Bool
isAlpha e1 e2 = normalize e1 == normalize e2
 where
  normalize e = foldr renameDef e (zip (bound e) (map singleton ['a'..]))
  renameDef (f,t) v@(Var x) = if f == x then Var t else v
  renameDef ft    (Abs h b) = Abs (renameDef ft h) (renameDef ft b)
  renameDef ft    (App g x) = App (renameDef ft g) (renameDef ft x)

isBound :: String -> Expr -> Bool
isBound x e = x `elem` bound e

var :: Expr -> [String]
var (Var x)   = [x]
var (Abs h b) = nub $ show h : var b
var (App f x) = nub $ var f ++ var x

bound :: Expr -> [String]
bound (Var x)   = []
bound (Abs h b) = nub $ show h : bound b
bound (App f x) = nub $ bound f ++ bound x

free :: Expr -> [String]
free e = var e \\ bound e

rename :: String -> Expr -> Expr
rename x v@(Var y) = if x == y then Var (y ++ "'") else v
rename x (Abs h b) = Abs (rename x h) (rename x b)
rename x (App g y) = App (rename x g) (rename x y)

replace :: Expr -> Expr -> Expr -> Expr
replace f t v@(Var _) = if f == v then t else v
replace f t (Abs h b) = Abs h (replace f t b)
replace f t (App g x) = App (replace f t g) (replace f t x)
