{-# OPTIONS_GHC -Wno-missing-methods #-}

module Lambdac.Syntax where

import Data.String


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

instance Num TExpr where
  fromInteger = TVar . fromInteger
  negate t = case t of
               TVar v   -> TVar (negate v)
               TAbs h b -> TAbs (negate h) (negate b)
               TApp f x -> TApp (negate f) (negate x)
