module Lambdac.Syntax where

import Data.String (IsString (..))

data Expr
  = Var String
  | Abs Expr Expr
  | App Expr Expr
  deriving Eq

λ :: Expr -> Expr -> Expr
λ = Abs

(∘) :: Expr -> Expr -> Expr
(∘) = App

instance IsString Expr where
  fromString = Var
