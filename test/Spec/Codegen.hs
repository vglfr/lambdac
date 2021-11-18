module Spec.Codegen where

import Lambdac.Codegen

import Data.ByteString as BS
import Test.Hspec

testCodegen :: Spec
testCodegen = describe "Lambdac.Codegen" $ do
  it "toIR" $ do
    ans <- toIR . toMod $ defAdd
    out <- BS.readFile "test/output/toIR.ll"
    ans `shouldBe` out

  it "toObj" $ do
    toObj . toMod $ defMain
    5 `shouldBe` 5
