module Spec.Codegen where

import Lambdac.Codegen

import Data.ByteString as BS
import Test.Hspec

testCodegen :: Spec
testCodegen = describe "Lambdac.Codegen" $ do
  it "toIR" $ do
    ans <- toIR module_
    out <- BS.readFile "test/output/toIR.ll"
    ans `shouldBe` out
