module Lambdac.Print.Repr where

import Lambdac.Syntax (Expr (..))


class Repr a where
  repr :: a -> String

instance Repr Expr where
  repr (Var x)   = "(Var \"" ++ x ++ "\")"
  repr (Abs h b) = "(Abs " ++ repr h ++ " " ++ repr b ++ ")"
  repr (App f x) = "(App " ++ repr f ++ " " ++ repr x ++ ")"
