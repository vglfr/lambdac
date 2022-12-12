module Spec.Print.VTree where

import Lambdac.Example.Abs (f1, f1v)
import Lambdac.Print.VTree (vtree)

import Test.Hspec (Spec, describe, it, shouldBe)

testPrintVTree :: Spec
testPrintVTree = describe "Lambdac.Print.VTree" $ do
  it "vtree Abs - λx.xxx - f1" $ do
    vtree f1 `shouldBe` f1v
