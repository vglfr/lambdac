module Spec.Print.VTree where

import Lambdac.Example.Abs (f1, f1v)
import Lambdac.Example.App (a1, a1v)
import Lambdac.Example.Var (v1, v2, v3, v1v, v2v, v3v)
import Lambdac.Print.VTree (vtree)

import Test.Hspec (Spec, describe, it, shouldBe)

testPrintVTree :: Spec
testPrintVTree = describe "Lambdac.Print.VTree" $ do
  it "vtree Var v1 (x)" $ do
    vtree v1 `shouldBe` v1v

  it "vtree Var v2 (x')" $ do
    vtree v2 `shouldBe` v2v

  it "vtree Var v3 (x'')" $ do
    vtree v3 `shouldBe` v3v

  it "vtree Abs f1 (λx.xxx)" $ do
    vtree f1 `shouldBe` f1v

  it "vtree App a1 (x x)" $ do
    vtree a1 `shouldBe` a1v
