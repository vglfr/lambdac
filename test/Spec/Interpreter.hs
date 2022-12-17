{-# LANGUAGE OverloadedStrings #-}

module Spec.Interpreter where

import Lambdac.Example.Abs (f4, f5, f4e, f5e)
import Lambdac.Example.App
  ( a3, a4, a7, a8, a9, a10, a11, a12, a13, a15, a16, a17
  , a3e, a4e, a7e, a8e, a9e, a10e, a11e, a12e, a13e, a15e, a16e, a17e)
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

  it "eval Abs f4 (λx.x -> λx.x) - irreducible" $ do
    eval f4 `shouldBe` f4e

  it "eval Abs f5 (λx.(λy.y)x -> λx.x) - reducible" $ do
    eval f5 `shouldBe` f5e

  it "eval App a3 (λx.x x -> x) - (Abs @ Var) bound same name" $ do
    eval a3 `shouldBe` a3e

  it "eval App a7 (λx.x y -> y) - (Abs @ Var) bound different name" $ do
    eval a7 `shouldBe` a7e

  it "eval App a8 (λx.z y -> z) - (Abs @ Var) free" $ do
    eval a8 `shouldBe` a8e

  it "eval App a9 (λxy.x u -> λy.u) - (Abs @ Var) nested" $ do
    eval a9 `shouldBe` a9e

  it "eval App a10 (λxy.y u -> λy.y) - (Abs @ Var) unused bound variable" $ do
    eval a10 `shouldBe` a10e

  it "eval App a11 (λxy.xy λz.(λc.v)z -> λy.v) - (Abs @ Abs) without alpha" $ do
    eval a11 `shouldBe` a11e

  it "eval App a12 (λxy.xy λz.(λc.y)z -> λy'.y) - (Abs @ Abs) with alpha" $ do
    eval a12 `shouldBe` a12e

  it "eval App a13 (λx.xx λx.xx -> λx.xx λx.xx) - (Abs @ Abs) Ω without alpha" $ do
    eval a13 `shouldBe` a13e

  it "eval App a4 (λx.xx λy.yy -> λy.yy λy.yy) - (Abs @ Abs) Ω with alpha" $ do
    eval a4 `shouldBe` a4e

  it "eval App a15 (λxy.x u v -> u) - (App @ Var)" $ do
    eval a15 `shouldBe` a15e

  it "eval App a16 (λxy.y u v -> v) - (App @ Var) to Var" $ do
    eval a16 `shouldBe` a16e

  it "eval App a17 (λxy.y u v w -> v w) - (App @ Var) to App" $ do
    eval a17 `shouldBe` a17e
