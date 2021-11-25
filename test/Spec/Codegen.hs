{-# LANGUAGE OverloadedStrings #-}

module Spec.Codegen where

import Lambdac.Codegen
import Lambdac.Printer
import Lambdac.Syntax

import Data.ByteString as BS
import LLVM.AST
import LLVM.AST.Global
import Test.Hspec

f1 :: Definition
f1 = GlobalDefinition functionDefaults
  { name = Name "f1"
  , parameters = ([Parameter str (Name "a1") []], False)
  , returnType = str
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
      (Name "entry")
      []
      (Do $ Ret (Just (LocalReference str (Name "a1"))) [])

testCodegen :: Spec
testCodegen = describe "Lambdac.Codegen" $ do
  it "toDef - abstraction" $ do
    toDef (λ "x" "x") `shouldBe` f1

  xit "toIR" $ do
    ans <- toIR . toMod $ defAdd
    out <- BS.readFile "test/output/toIR.ll"
    ans `shouldBe` out

  xit "toObj" $ do
    toObj . toMod $ defMain
    5 `shouldBe` 5

  it "toBin" $ do
    toBin . toMod $ defMain
    5 `shouldBe` 5
