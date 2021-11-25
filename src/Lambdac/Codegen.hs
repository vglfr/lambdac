{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Codegen where

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Constant as C
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module as LLVM
import LLVM.Target

import Control.Monad (void)
import Control.Monad.Except
import Data.ByteString.Char8 as BS
import Data.ByteString
import System.Process

import Lambdac.Syntax

-- result to stdout
-- bootstrap main
-- nested abstractions

i8 :: Type
i8 = IntegerType 8

i32 :: Type
i32 = IntegerType 32

str :: Type
str = ArrayType 1 i8

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter i32 (Name "a") []
        , Parameter i32 (Name "b") [] ]
      , False )
  , returnType = i32
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            AST.Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference i32 (Name "a"))
                (LocalReference i32 (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference i32 (Name "result"))) [])

defMain :: Definition
defMain = GlobalDefinition functionDefaults
  { name = Name "main"
  , returnType = i32
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [Name "result" :=
            AST.Add False  -- no signed wrap
                False  -- no unsigned wrap
                (ConstantOperand (C.Int 32 1))
                (ConstantOperand (C.Int 32 1))
                []]
        (Do $ Ret (Just (LocalReference i32 (Name "result"))) [])

toDef :: Expr -> Definition
toDef (Abs (Var h) (Var b)) = GlobalDefinition functionDefaults
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

toMod :: Definition -> AST.Module
toMod def = defaultModule
  { moduleName = "main"
  , moduleDefinitions = [def]
  }

toIR :: AST.Module -> IO ByteString
toIR mod = withContext $ \ctx -> withModuleFromAST ctx mod moduleLLVMAssembly

toObj :: AST.Module -> IO ()
toObj ast = do
  withContext $ \ctx ->
    withModuleFromAST ctx ast $ \llvm ->
      withHostTargetMachineDefault $ \target -> do
        writeObjectToFile target (File "bin/test.o") llvm

toBin :: AST.Module -> IO ()
toBin ast = do
  withContext $ \ctx ->
    withModuleFromAST ctx ast $ \llvm ->
      withHostTargetMachineDefault $ \target -> do
        writeObjectToFile target (File "bin/test.o") llvm
        void $ readProcess "gcc" ["bin/test.o", "-o", "bin/a.out"] ""
