{-# LANGUAGE OverloadedStrings #-}

module Spec.Interpreter where

import Lambdac.Example.Abs (f4, f4e)
import Lambdac.Example.Var (v1, v2, v3, v1e, v2e, v3e)
import Lambdac.Interpreter (eval)
import Lambdac.Syntax (λ, (∘))

import Test.Hspec (Spec, describe, it, shouldBe)

testInterpreter :: Spec
testInterpreter = describe "Lambdac.Interpreter" $ do
  it "eval Var v1 (x -> x)" $ do
    eval v1 `shouldBe` v1e

  it "eval Var v2 (x' -> x')" $ do
    eval v2 `shouldBe` v2e

  it "eval Var v3 (x'' -> x'')" $ do
    eval v3 `shouldBe` v3e

  it "eval Abs f4 (λx.x -> λx.x)" $ do
    eval f4 `shouldBe` f4e

  it "eval Abs - reducible" $ do
    eval (λ "x" (λ "y" "y" ∘ "x")) `shouldBe` (λ "x" "x")

  it "eval Abs / Var - bound same name" $ do
    eval (λ "x" "x" ∘ "x") `shouldBe` "x"

  it "eval Abs / Var - bound different name" $ do
    eval (λ "x" "x" ∘ "y") `shouldBe` "y"

  it "eval Abs / Var - free" $ do
    eval (λ "x" "z" ∘ "y") `shouldBe` "z"

  it "eval Abs / Var - nested" $ do
    eval (λ "x" (λ "y" "x") ∘ "u") `shouldBe` (λ "y" "u")

  it "eval Abs / Var - unused bound variable" $ do
    eval (λ "x" (λ "y" "y") ∘ "u") `shouldBe` (λ "y" "y")

  it "eval Abs / Abs - without alpha" $ do
    eval (λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "z" (λ "c" "v" ∘ "z")) `shouldBe` (λ "y" "v")

  it "eval Abs / Abs - with alpha" $ do
    eval (λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "z" (λ "c" "y" ∘ "z")) `shouldBe` (λ "y'" "y")

  it "eval Abs / Abs - Ω without alpha" $ do
    eval (λ "x" ("x" ∘ "x") ∘ λ "x" ("x" ∘ "x")) `shouldBe` (λ "x" ("x" ∘ "x") ∘ λ "x" ("x" ∘ "x"))

  it "eval Abs / Abs - Ω with alpha" $ do
    eval (λ "x" ("x" ∘ "x") ∘ λ "y" ("y" ∘ "y")) `shouldBe` (λ "y" ("y" ∘ "y") ∘ λ "y" ("y" ∘ "y"))

  it "eval App / Var" $ do
    eval (λ "x" (λ "y" "x") ∘ "u" ∘ "v") `shouldBe` "u"

  it "eval App / Var - to Var" $ do
    eval (λ "x" (λ "y" "y") ∘ "u" ∘ "v") `shouldBe` "v"

  it "eval App / Var - to App" $ do
    eval (λ "x" (λ "y" "y") ∘ "u" ∘ "v" ∘ "w") `shouldBe` ("v" ∘ "w")
