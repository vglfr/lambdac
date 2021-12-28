{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Combinators where

import Lambdac.Syntax

_I :: Expr
_I = λ "x" "x"

_K :: Expr
_K = λ "x" (λ "y" "x")

_S :: Expr
_S = λ "x" (λ "y" (λ "z" ("x" • "z" • ("y" • "z"))))

_B :: Expr
_B = λ "x" (λ "y" (λ "z" ("x" • ("y" • "z"))))

_C :: Expr
_C = λ "x" (λ "y" (λ "z" ("x" • "z" • "y")))

_W :: Expr
_W = λ "x" (λ "y" ("x" • "y" • "y"))

_U :: Expr
_U = λ "x" ("x" • "x")

_ω :: Expr
_ω = λ "x" ("x" • "x")

_Ω :: Expr
_Ω = _ω • _ω

_Y :: Expr
_Y = λ "g" (λ "x" ("g" • ("x" • "x")) • λ "x" ("g" • ("x" • "x")))
