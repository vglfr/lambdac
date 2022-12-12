module Spec.Print.HTree where

import Lambdac.Example.Abs (f1, f1h)
import Lambdac.Print.HTree (htree)

import Test.Hspec (Spec, describe, it, shouldBe)

testPrintHTree :: Spec
testPrintHTree = describe "Lambdac.Print.HTree" $ do
  it "htree Abs - λx.xxx - f1" $ do
    htree f1 `shouldBe` f1h
