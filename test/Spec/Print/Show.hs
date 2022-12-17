{-# LANGUAGE OverloadedStrings #-}

module Spec.Print.Show where

import Lambdac.Example.Abs (f1, f1p)
import Lambdac.Example.App (a1, a1p)
import Lambdac.Example.Var (v1, v2, v3, v1p, v2p, v3p)
import Lambdac.Print.Show ()
import Lambdac.Syntax (Expr, λ, (∘))

import Test.Hspec (Spec, describe, it, shouldBe)

testPrintShow :: Spec
testPrintShow = describe "Lambdac.Print.Show" $ do
  it "show Var v1 (x)" $ do
    show v1 `shouldBe` v1p

  it "show Var v2 (x')" $ do
    show v2 `shouldBe` v2p

  it "show Var v3 (x'')" $ do
    show v3 `shouldBe` v3p

  it "show Abs - one argument" $ do
    show (λ "x" "x") `shouldBe` "λx.x"

  it "show Abs - two arguments" $ do
    show (λ "x" (λ "y" "x")) `shouldBe` "λxy.x"

  it "show Abs - three arguments" $ do
    show (λ "x" (λ "y" (λ "z" "x"))) `shouldBe` "λxyz.x"

  it "show Abs - application in body" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ∘ "y" ∘ "z")))) `shouldBe` "λxyz.xyz"

  it "show Abs - abstraction in start of body" $ do
    show (λ "x" (λ "y" (λ "z" (λ "u" "v" ∘ "y" ∘ "z")))) `shouldBe` "λxyz.(λu.v)yz"

  it "show Abs - abstraction in middle of body" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ∘ λ "u" "v" ∘ "z")))) `shouldBe` "λxyz.x(λu.v)z"

  it "show Abs - abstraction in end of body" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ∘ "y" ∘ λ "u" "v")))) `shouldBe` "λxyz.xy(λu.v)"

  it "show Abs - λxyz.x(λuv.v)z" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ∘ λ "u" (λ "v" "v") ∘ "z")))) `shouldBe` "λxyz.x(λuv.v)z"

  it "show Abs - λxyz.x(λu.(λv.v)w)z" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ∘ λ "u" (λ "v" "v" ∘ "w") ∘ "z")))) `shouldBe` "λxyz.x(λu.(λv.v)w)z"

  it "show Abs - λxyz.x(λuv.vw)z" $ do
    show (λ "x" (λ "y" (λ "z" ("x" ∘ λ "u" (λ "v" ("v" ∘ "w")) ∘ "z")))) `shouldBe` "λxyz.x(λuv.vw)z"

  it "show Abs f1 (λx.xxx)" $ do
    show f1 `shouldBe` f1p

  it "show App a1 (x x)" $ do
    show a1 `shouldBe` a1p

  it "show App - three variables" $ do
    show ("x" ∘ "y" ∘ "z") `shouldBe` "x y z"

  it "show App - abstraction and variable" $ do
    show (λ "x" "x" ∘ "x") `shouldBe` "λx.x x"

  it "show App - variable and abstraction" $ do
    show ("x" ∘ λ "x" "x") `shouldBe` "x λx.x"

  it "show App - two abstractions" $ do
    show (λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "z" (λ "c" "y" ∘ "z")) `shouldBe` "λxy.xy λz.(λc.y)z"

  it "show App - three abstractions" $ do
    show (λ "x" "x" ∘ λ "y" "y" ∘ λ "z" "z") `shouldBe` "λx.x λy.y λz.z"
