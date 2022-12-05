module Lambdac.Print.Show where

import GHC.Show (showSpace)
import Lambdac.Syntax (Expr (..))

instance Show Expr where
  showsPrec _ (Var x)   = showString x
  showsPrec _ (Abs h b) = showString "λ" . shows h . showAbs b
   where
    showAbs (Abs h' b') = shows h' . showAbs b'
    showAbs e           = showString "." . showInner e
    showInner e@(Abs _ _) = showParen True $ shows e
    showInner (App f x)   = showInner f . showInner x
    showInner e           = shows e
  showsPrec _ (App f x) = shows f . showSpace . shows x
