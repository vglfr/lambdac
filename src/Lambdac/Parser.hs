module Lambdac.Parser where

import Lambdac.Syntax

import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Text.Parsec (Parsec, char, chainl1, many, many1, oneOf, parse, skipMany1, space, string, try, (<|>))

data Command
  = PPrint Expr -- :p
  | VTree Expr  -- :v
  | HTree Expr  -- :h
  | Repr Expr   -- :r
  | Type Expr   -- :t
  | Eval Expr   -- :e (default)
  | Help        -- :h
  | NOP

parseLine :: String -> IO Command
parseLine = pure . fromRight NOP . parse parseCommand ""

parseCommand :: Parsec String () Command
parseCommand = try parsePPrint <|> try parseVTree <|> try parseHTree <|> try parseRepr <|> parseEval

parsePPrint :: Parsec String () Command
parsePPrint = string ":p " >> PPrint <$> parseExpr

parseHTree :: Parsec String () Command
parseHTree = string ":h " >> HTree <$> parseExpr

parseVTree :: Parsec String () Command
parseVTree = string ":v " >> VTree <$> parseExpr

parseRepr :: Parsec String () Command
parseRepr = string ":r " >> Repr <$> parseExpr

parseEval :: Parsec String () Command
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
