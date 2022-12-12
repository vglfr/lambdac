module Spec.Print.Repr where

import Lambdac.Example.Abs (f1, f1r)
import Lambdac.Print.Repr (repr)

import Test.Hspec (Spec, describe, it, shouldBe)

testPrintRepr :: Spec
testPrintRepr = describe "Lambdac.Print.Repr" $ do
  it "repr Abs - λx.xxx - f1" $ do
    repr f1 `shouldBe` f1r
