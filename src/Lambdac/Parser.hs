module Lambdac.Parser where

import Lambdac.Syntax

import Data.Either (fromRight)
import Text.Parsec

parseLE :: String -> IO Expr
parseLE = pure . fromRight (Var "e") . parse parseVar ""

parseVar :: Parsec String () Expr
parseVar = Var <$> (lowerS <> primeS)
 where
  lowerS = pure <$> lower
  primeS = option "" (pure <$> char '\'')
