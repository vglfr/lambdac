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
