module Lambdac.Parser where

import Lambdac.Syntax

import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Functor (($>))
-- import Debug.Trace
import Text.Parsec
-- import Text.Parsec.Token (parens)

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
parseAbs = Abs <$> parseHead <*> parseTail
 where
  parseHead = char 'λ' *> parseVar
  parseTail = parseAbsCont <|> parseBody
  parseAbsCont = Abs <$> parseVar <*> parseTail
  parseBody = do
    _  <- char '.'
    vs <- many1 (parseAbsParen <|> parseVar)
    case length vs of
      1 -> pure $ head vs
      _ -> pure $ foldl' App (head vs) (tail vs)
  parseAbsParen = char '(' *> parseAbs <* char ')'
  -- parseAbsParen :: Parsec String () Expr
  -- parseAbsParen = parens parseAbs

parseApp :: Parsec String () Expr
parseApp = parseExpr' `chainl1` parseSpace'
 where
  parseExpr'  = parseAbs <|> parseVar
  parseSpace' = skipMany1 space $> App
