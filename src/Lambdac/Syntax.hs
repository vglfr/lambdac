module Lambdac.Syntax where

data Expr
  = Var String
  | Abs Expr Expr
  | App Expr Expr
  deriving Eq

λ :: Expr -> Expr -> Expr
λ = Abs

(⋅) :: Expr -> Expr -> Expr
(⋅) = App
