{-# LANGUAGE OverloadedStrings #-}

module Spec.Parser where

import Lambdac.Parser
import Lambdac.Print.Show
import Lambdac.Syntax

import Data.ByteString as BS
import Data.Either (isLeft)
import Test.Hspec
import Text.Parsec

testParser :: Spec
testParser = do
  testParseVar
  testParseAbs
  testParseApp

testParseVar :: Spec
testParseVar = describe "Lambdac.Parser - Var" $ do
  let parseVar' = parse (parseVar <* eof) "Error parsing variable"

  it "parseVar - one letter (v1)" $ do
    parseVar' "x" `shouldBe` Right (Var "x")

  it "parseVar - one prime" $ do
    parseVar' "x'" `shouldBe` Right (Var "x'")

  it "parseVar - two prime" $ do
    parseVar' "x''" `shouldBe` Right (Var "x''")

  it "parseVar - two letters" $ do
    parseVar' "xy" `shouldSatisfy` isLeft

  it "parseVar - uppercase letter" $ do
    parseVar' "X" `shouldSatisfy` isLeft

  it "parseVar - lambda" $ do
    parseVar' "λ" `shouldSatisfy` isLeft

testParseAbs :: Spec
testParseAbs = describe "Lambdac.Parser - Abs" $ do
  let parseAbs' = parse (parseAbs <* eof) "Error parsing abstraction"

  it "parseAbs - one argument (f1)" $ do
    parseAbs' "λx.x" `shouldBe` Right (λ "x" "x")

  it "parseAbs - two arguments (f2)" $ do
    parseAbs' "λxy.x" `shouldBe` Right (λ "x" (λ "y" "x"))

  it "parseAbs - three arguments (f3)" $ do
    parseAbs' "λxyz.x" `shouldBe` Right (λ "x" (λ "y" (λ "z" "x")))

  it "parseAbs - application in body (f4)" $ do
    parseAbs' "λxyz.xyz" `shouldBe` Right (λ "x" (λ "y" (λ "z" ("x" ∘ "y" ∘ "z"))))

  it "parseAbs - abstraction in start of body" $ do
    parseAbs' "λxyz.(λu.v)yz" `shouldBe` Right (λ "x" (λ "y" (λ "z" (λ "u" "v" ∘ "y" ∘ "z"))))

  it "parseAbs - abstraction in middle of body" $ do
    parseAbs' "λxyz.x(λu.v)z" `shouldBe` Right (λ "x" (λ "y" (λ "z" ("x" ∘ λ "u" "v" ∘ "z"))))

  it "parseAbs - abstraction in end of body" $ do
    parseAbs' "λxyz.xy(λu.v)" `shouldBe` Right (λ "x" (λ "y" (λ "z" ("x" ∘ "y" ∘ λ "u" "v"))))

  it "parseAbs - no lambda" $ do
    parseAbs' "x.x" `shouldSatisfy` isLeft

  it "parseAbs - no dot" $ do
    parseAbs' "λxx" `shouldSatisfy` isLeft

testParseApp :: Spec
testParseApp = describe "Lambdac.Parser - App" $ do
  let parseApp' = parse (parseApp <* eof) "Error parsing application"

  it "parseApp - two variables (a1)" $ do
    parseApp' "x x" `shouldBe` Right ("x" ∘ "x")

  it "parseApp - three variables (a2)" $ do
    parseApp' "x x x" `shouldBe` Right ("x" ∘ "x" ∘ "x")

  it "parseApp - abstraction and variable (a3)" $ do
    parseApp' "λx.x x" `shouldBe` Right (λ "x" "x" ∘ "x")

  it "parseApp - variable and abstraction" $ do
    parseApp' "x λx.x" `shouldBe` Right ("x" ∘ λ "x" "x")

  it "parseApp - two abstractions" $ do
    parseApp' "λx.x λx.x" `shouldBe` Right (λ "x" "x" ∘ λ "x" "x")

  it "parseApp - three abstractions" $ do
    parseApp' "λx.x λx.x λx.x" `shouldBe` Right (λ "x" "x" ∘ λ "x" "x" ∘ λ "x" "x")

  it "parseApp - trailing space" $ do
    parseApp' "x " `shouldSatisfy` isLeft

  it "parseApp - leading space" $ do
    parseApp' " x" `shouldSatisfy` isLeft
