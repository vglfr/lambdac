module Spec.Print.HTree where

import Lambdac.Example.Abs (f1, f1h)
import Lambdac.Example.App (a1, a1h)
import Lambdac.Example.Var (v1, v2, v3, v1h, v2h, v3h)
import Lambdac.Print.HTree (htree)

import Test.Hspec (Spec, describe, it, shouldBe)

testPrintHTree :: Spec
testPrintHTree = describe "Lambdac.Print.HTree" $ do
  it "htree Var v1 (x)" $ do
    htree v1 `shouldBe` v1h

  it "htree Var v2 (x')" $ do
    htree v2 `shouldBe` v2h

  it "htree Var v3 (x'')" $ do
    htree v3 `shouldBe` v3h

  it "htree Abs f1 (λx.xxx)" $ do
    htree f1 `shouldBe` f1h

  it "htree Abs a1 (x x)" $ do
    htree a1 `shouldBe` a1h
