module Lambdac.Parser where

import Lambdac.Syntax

import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Functor (($>))
-- import Debug.Trace
import Text.Parsec
-- import Text.Parsec.Token (parens)

data Repl
  = PPrint Expr
  | Tree Expr
  | Repr Expr
  | Eval Expr

parseLine :: String -> IO Repl
parseLine = pure . fromRight (Tree (Var "e")) . parse parseRepl ""

parseRepl :: Parsec String () Repl
parseRepl = try parsePPrint <|> try parseTree <|> try parseRepr <|> parseEval

parsePPrint :: Parsec String () Repl
parsePPrint = string ":p " >> PPrint <$> parseExpr

parseTree :: Parsec String () Repl
parseTree = string ":t " >> Tree <$> parseExpr

parseRepr :: Parsec String () Repl
parseRepr = string ":r " >> Repr <$> parseExpr

parseEval :: Parsec String () Repl
parseEval = Eval <$> parseExpr

parseExpr :: Parsec String () Expr
parseExpr = try parseApp <|> try parseAbs <|> parseVar

parseVar :: Parsec String () Expr
parseVar = Var <$> (base <> primes)
 where
  base   = pure <$> oneOf ['a'..'z']
  primes = many (char '\'')

parseAbs :: Parsec String () Expr
parseAbs = Abs <$> parseHead <*> parseTail
 where
  parseHead = oneOf "λ\\" *> parseVar
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
