module Lambdac.Printer where

import Lambdac.Syntax

import Data.String (IsString (..))

instance IsString Expr where
  fromString = Var

instance Show Expr where
  show (Var x)   = x
  show (Abs h b) = "λ" ++ show h ++ showAbs b
   where
    showAbs (Abs h' b') = show h' ++ showAbs b'
    showAbs e           = "." ++ showInner e
    showInner e@(Abs _ _) = "(" ++ show e ++ ")"
    showInner (App f x)   = showInner f ++ showInner x
    showInner e           = show e
  show (App f x) = show f ++ " " ++ show x

class Repr a where
  repr :: a -> String

instance Repr Expr where
  repr (Var x)   = "(Var \"" ++ x ++ "\")"
  repr (Abs h b) = "(Abs " ++ repr h ++ " " ++ repr b ++ ")"
  repr (App f x) = "(App " ++ repr f ++ " " ++ repr x ++ ")"
