module Quickcheck where

import Control.Applicative

import Lambdac.Parser
import Lambdac.Print.Show
import Lambdac.Syntax

import Text.Parsec
import Test.QuickCheck

expr :: Gen Expr
expr = sized expr'
expr' 0 = Var <$> elements ["x", "y", "z", "u", "v"]
expr' n | n > 0 = 
  oneof [ Var <$> elements ["x", "y", "z", "u", "v"]
        , liftA2 Abs (Var <$> elements ["x", "y", "z", "u", "v"]) subexpr
        , liftA2 App subexpr subexpr ]
  where subexpr = expr' (n `div` 2)

instance Arbitrary Expr where
  arbitrary = expr

temp2 :: Expr -> Bool
temp2 t = parse parseExpr "" (show t) == Right t
