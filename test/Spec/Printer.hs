{-# LANGUAGE OverloadedStrings #-}

module Spec.Printer where

import Lambdac.Printer
import Lambdac.Syntax ((⋅), λ, Expr(Var))

import Data.ByteString as BS
import Test.Hspec

testPrinter :: Spec
testPrinter = describe "Lambdac.Printer" $ do
  it "show Var - base" $ do
    show (Var "x") `shouldBe` "x"

  it "show Var - base one prime" $ do
    show (Var "x'") `shouldBe` "x'"

  it "show Var - base two primes" $ do
    show (Var "x''") `shouldBe` "x''"

  it "show Abs - one argument" $ do
    show (λ "x" "x") `shouldBe` "λx.x"

  it "show Abs - two arguments" $ do
    show (λ "x" (λ "y" "x")) `shouldBe` "λxy.x"

  it "show Abs - three arguments" $ do
    show (λ "x" (λ "y" (λ "z" "x"))) `shouldBe` "λxyz.x"

  it "show Abs - variable application in body" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ⋅ "y" ⋅ "z")))) `shouldBe` "λxyz.xyz"

  it "show Abs - abstraction in body" $ do
    show (λ "x" (λ "y" (λ "z" "x" ⋅ "y" ⋅ "z"))) `shouldBe` "λxy.(λz.x)yz"

  it "show Abs - abstraction in start of body" $ do
    show (λ "x" (λ "y" (λ "z" (λ "u" "v" ⋅ "y" ⋅ "z")))) `shouldBe` "λxyz.(λu.v)yz"

  it "show Abs - abstraction in middle of body" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" "v" ⋅ "z")))) `shouldBe` "λxyz.x(λu.v)z"

  it "show Abs - abstraction in end of body" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ⋅ "y" ⋅ λ "u" "v")))) `shouldBe` "λxyz.xy(λu.v)"

  it "show Abs - λxyz.x(λuv.v)z" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" "v") ⋅ "z")))) `shouldBe` "λxyz.x(λuv.v)z"

  it "show Abs - λxyz.x(λu.(λv.v)w)z" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" "v" ⋅ "w") ⋅ "z")))) `shouldBe` "λxyz.x(λu.(λv.v)w)z"

  it "show Abs - λxyz.x(λuv.vw)z" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" ("v" ⋅ "w")) ⋅ "z")))) `shouldBe` "λxyz.x(λuv.vw)z"

  it "show App - two variables" $ do
    show ("x" ⋅ "y") `shouldBe` "x y"

  it "show App - three variables" $ do
    show ("x" ⋅ "y" ⋅ "z") `shouldBe` "x y z"

  it "show App - abstraction and variable" $ do
    show (λ "x" "x" ⋅ "x") `shouldBe` "λx.x x"

  it "show App - variable and abstraction" $ do
    show ("x" ⋅ λ "x" "x") `shouldBe` "x λx.x"

  it "show App - two abstractions" $ do
    show (λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "y" ⋅ "z")) `shouldBe` "λxy.xy λz.(λc.y)z"

  it "show App - three abstractions" $ do
    show (λ "x" "x" ⋅ λ "y" "y" ⋅ λ "z" "z") `shouldBe` "λx.x λy.y λz.z"
