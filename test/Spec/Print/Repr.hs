module Spec.Print.Repr where

import Lambdac.Example.Abs (f1, f1r)
import Lambdac.Example.Var (v1, v2, v3, v1r, v2r, v3r)
import Lambdac.Print.Repr (repr)

import Test.Hspec (Spec, describe, it, shouldBe)

testPrintRepr :: Spec
testPrintRepr = describe "Lambdac.Print.Repr" $ do
  it "repr Var v1 (x)" $ do
    repr v1 `shouldBe` v1r

  it "repr Var v2 (x')" $ do
    repr v2 `shouldBe` v2r

  it "repr Var v3 (x'')" $ do
    repr v3 `shouldBe` v3r

  it "repr Abs f1 (λx.xxx)" $ do
    repr f1 `shouldBe` f1r
