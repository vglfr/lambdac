{-# LANGUAGE OverloadedStrings #-}

module Spec.Interpreter where

import Lambdac.Interpreter
import Lambdac.Syntax

import Data.ByteString as BS
import Test.Hspec

testInterpreter :: Spec
testInterpreter = describe "Lambdac.Interpreter" $ do
  it "Var" $ do
    eval "x" `shouldBe` "x"

  it "Abs - unreducible" $ do
    eval (λ "x" "x") `shouldBe` (λ "x" "x")

  it "Abs - reducible" $ do
    eval (λ "x" (λ "y" "y" ⋅ "x")) `shouldBe` (λ "x" "x")

  it "Abs / Var - bound same name" $ do
    eval (λ "x" "x" ⋅ "x") `shouldBe` "x"

  it "Abs / Var - bound different name" $ do
    eval (λ "x" "x" ⋅ "y") `shouldBe` "y"

  it "Abs / Var - free" $ do
    eval (λ "x" "z" ⋅ "y") `shouldBe` "z"

  it "Abs / Var - nested" $ do
    eval (λ "x" (λ "y" "x") ⋅ "u") `shouldBe` (λ "y" "u")

  it "Abs / Var - unused bound variable" $ do
    eval (λ "x" (λ "y" "y") ⋅ "u") `shouldBe` (λ "y" "y")

  it "Abs / Abs - without alpha" $ do
    eval (λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "v" ⋅ "z")) `shouldBe` (λ "y" "v")

  it "Abs / Abs - with alpha" $ do
    eval (λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "y" ⋅ "z")) `shouldBe` (λ "y'" "y")

  it "Abs / Abs - Ω without alpha" $ do
    eval (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")) `shouldBe` (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x"))

  it "Abs / Abs - Ω with alpha" $ do
    eval (λ "x" ("x" ⋅ "x") ⋅ λ "y" ("y" ⋅ "y")) `shouldBe` (λ "y" ("y" ⋅ "y") ⋅ λ "y" ("y" ⋅ "y"))

  it "App / Var" $ do
    eval (λ "x" (λ "y" "x") ⋅ "u" ⋅ "v") `shouldBe` "u"

  it "App / Var - to Var" $ do
    eval (λ "x" (λ "y" "y") ⋅ "u" ⋅ "v") `shouldBe` "v"

  it "App / Var - to App" $ do
    eval (λ "x" (λ "y" "y") ⋅ "u" ⋅ "v" ⋅ "w") `shouldBe` ("v" ⋅ "w")
