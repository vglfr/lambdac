{-# LANGUAGE OverloadedStrings #-}

module Spec.Parser where

import Lambdac.Parser
import Lambdac.Printer
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

  it "parseVar - one letter" $ do
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

  it "parseAbs - simple" $ do
    parseAbs' "λx.x" `shouldBe` Right (λ "x" "x")

  it "parseAbs - no lambda" $ do
    parseAbs' "x.x" `shouldSatisfy` isLeft

  it "parseAbs - no dot" $ do
    parseAbs' "λxx" `shouldSatisfy` isLeft

testParseApp :: Spec
testParseApp = describe "Lambdac.Parser - App" $ do
  let parseApp' = parse (parseApp <* eof) "Error parsing application"

  it "parseApp - two variables" $ do
    parseApp' "x x" `shouldBe` Right ("x" ⋅ "x")

  it "parseApp - three variables" $ do
    parseApp' "x x x" `shouldBe` Right ("x" ⋅ "x" ⋅ "x")

  it "parseApp - abstraction and variable" $ do
    parseApp' "λx.x x" `shouldBe` Right (λ "x" "x" ⋅ "x")

  it "parseApp - variable and abstraction" $ do
    parseApp' "x λx.x" `shouldBe` Right ("x" ⋅ λ "x" "x")

  it "parseApp - two abstractions" $ do
    parseApp' "λx.x λx.x" `shouldBe` Right (λ "x" "x" ⋅ λ "x" "x")

  it "parseApp - three abstractions" $ do
    parseApp' "λx.x λx.x λx.x" `shouldBe` Right (λ "x" "x" ⋅ λ "x" "x" ⋅ λ "x" "x")

    -- (λx.x)(λy.y)
    -- (λx.x)(λy.y)z
    -- ((λx.x)(λy.y))z
    -- λxy.xy
    -- λx.(λy.xy)
    -- (λxy.xxy)(λx.xy)(λx.xz)
    -- (λxyz.xz(yz))(λnn.n)(λp.p)
    -- (λy.λz(λn.λn.n)z(yz))(λp.p)
    -- λz(λn.z)((λp.p)z)
    -- λxy.xz
    -- λxy.xxy
    -- λxyz.zx
    -- (λx.xxx)z
    -- (λabc.cba)zz(λwv.w)
    -- (λz.zz)(λy.yy)
    -- λx.xxx
    -- λxy.zx
    -- λxyz.xy(zx)
    -- λxyz.xy(zxy)
    -- λxy.xy(zxy)
    -- (λx.λy.xyy)(λa.a)b
    -- (λy.y)(λx.xx)(λz.zq)
    -- (λz.z)(λz.zz)(λz.zy)
    -- (λx.λy.xyy)(λy.y)y
    -- (λa.aa)(λb.ba)c
    -- (λxyz.xz(yz))(λx.z)(λx.a)
