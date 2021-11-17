module Lambdac.Parser where

import Lambdac.Syntax

import Data.Either (fromRight)
import Data.Functor (($>))
-- import Debug.Trace
import Text.Parsec

parseLE :: String -> IO Expr
parseLE = pure . fromRight (Var "e") . parse parseVar ""

parseExpr :: Parsec String () Expr
parseExpr = try parseApp <|> try parseAbs <|> try parseVar

parseVar :: Parsec String () Expr
parseVar = Var <$> (base <> primes)
 where
  base   = pure <$> oneOf ['a'..'z']
  primes = many (char '\'')

parseAbs :: Parsec String () Expr
parseAbs = Abs <$> parseHead <*> parseBody
 where
  parseHead = char 'λ' *> parseVar
  parseBody = skipMany (char '.') *> (parseVar <|> parseAbs)

parseApp :: Parsec String () Expr
parseApp = parseExpr' `chainl1` parseSpace'
 where
  parseExpr'  = parseAbs <|> parseVar
  parseSpace' = skipMany1 space $> App
