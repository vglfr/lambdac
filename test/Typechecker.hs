module Typechecker where

import Lambdac.Example.Var (v1, v2, v3, v1', v2', v3', v1t, v2t, v3t)
import Lambdac.Typechecker (tcheck, ttree)
import Lambdac.Syntax (λ, (∘))

import Test.Hspec (Spec, describe, it, shouldBe)

testTypechecker :: Spec
testTypechecker = do
  testTTree
  testTCheck

testTTree :: Spec
testTTree = describe "Lambdac.Typechecker - TTree" $ do
  it "ttree Var v1 (x)" $ do
    ttree v1 `shouldBe` v1'

  it "ttree Var v2 (x')" $ do
    ttree v2 `shouldBe` v2'

  it "ttree Var v3 (x'')" $ do
    ttree v3 `shouldBe` v3'

testTCheck :: Spec
testTCheck = describe "Lambdac.Typechecker - TCheck" $ do
  it "tcheck Var v1 (0 -> 0)" $ do
    tcheck v1' `shouldBe` v1t

  it "tcheck Var v2 (0 -> 0)" $ do
    tcheck v2' `shouldBe` v2t

  it "tcheck Var v3 (0 -> 0)" $ do
    tcheck v3' `shouldBe` v3t

