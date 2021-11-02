{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

data Expr a
  = Var a
  | Lam (Expr a) (Expr a)
  | App (Expr a) (Expr a)
  deriving Eq

instance Show (Expr String) where
  show (Var x)   = x
  show (Lam v e) = "\\" ++ show v ++ go e
   where
    go (Lam v' e')  = " " ++ show v' ++ go e'
    go e'@(App _ _) = "." ++ showA e'
    go e'           = "." ++ show e'
    showA (App f x) = show f ++ showA x
    showA e'        = show e'
  show (App f x) = show f ++ " " ++ show x

eval :: Expr String -> IO (Expr String)
eval e = do
  print e
  case step e of
    Just e' -> eval e'
    Nothing -> do
      putStrLn "eof"
      pure e

step :: Expr String -> Maybe (Expr String)
step (Var x)   = Nothing
step (Lam v e) = Nothing
step (App f x) = case f of
  Lam v e -> Just $ sub v x e
  _       -> Nothing

sub :: Expr String -> Expr String -> Expr String -> Expr String
sub v x e = case e of
  v'@(Var _) -> if v == v' then x else v'
  Lam v' e'  -> Lam v' (sub v x e')
  App f x'   -> App (sub v x f) (sub v x x')

main :: IO (Expr String)
main = do
  print $ Var "x"
  print $ Lam (Var "x") (Var "x")
  print $ Lam (Var "x") (Lam (Var "y") (Var "y"))
  print $ Lam (Var "x") (Lam (Var "y") (Lam (Var "z") (Var "z")))
  print $ App (Lam (Var "x") (Var "x")) (Var "y")

  eval $ Var "x"
  eval $ App (Lam (Var "x") (Var "x")) (Var "y")
  eval $ App (Lam (Var "x") (Var "z")) (Var "y")
  eval $ App (Lam (Var "x") (Lam (Var "y") (Var "x"))) (Var "z")
  eval $ App (Lam (Var "x") (Lam (Var "y") (App (Var "x") (Var "x")))) (Var "z")
  eval $ App (Lam (Var "x") (Lam (Var "y") (App (Var "x") (Var "y")))) (Lam (Var "z") (App (Lam (Var "c") (Var "u")) (Var "z")))
  -- eval $ App (Lam (Var "x") (Lam (Var "y") (App (Var "x") (Var "y")))) (Lam (Var "z") (App (Lam (Var "c") (Var "y")) (Var "z")))

-- (λ "x" (λ "y" ("x" · "y"))) · (λ "z" ((λ "c" "y") · "z"))
