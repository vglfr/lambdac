module Spec.Interpreter where

import Lambdac.Interpreter

import Data.ByteString as BS
import Test.Hspec

testInterpreter :: Spec
testInterpreter = describe "Lambdac.Interpreter" $ do
  it "test" $ do
    pending
