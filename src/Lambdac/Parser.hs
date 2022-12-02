module Lambdac.Parser where

import Lambdac.Syntax

import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Text.Parsec (Parsec, char, chainl1, many, many1, oneOf, parse, skipMany1, space, string, try, (<|>))

data Repl
  = PPrint Expr
  | VTree Expr
  | HTree Expr
  | Repr Expr
  | Eval Expr
  | NOP

parseLine :: String -> IO Repl
parseLine = pure . fromRight NOP . parse parseRepl ""

parseRepl :: Parsec String () Repl
parseRepl = try parsePPrint <|> try parseVTree <|> try parseHTree <|> try parseRepr <|> parseEval

parsePPrint :: Parsec String () Repl
parsePPrint = string ":p " >> PPrint <$> parseExpr

parseHTree :: Parsec String () Repl
parseHTree = string ":h " >> HTree <$> parseExpr

parseVTree :: Parsec String () Repl
parseVTree = string ":v " >> VTree <$> parseExpr

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
