{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Combinators where

import Lambdac.Syntax

{- λx.x -> λx.x

  λ
 / \
x   x
-}
i :: Expr
i = λ "x" "x"

{- λxy.x -> λxy.x

  λ
 / \
x   λ
   / \
  y   x
-}
k :: Expr
k = λ "x" (λ "y" "x")

s :: Expr
s = λ "x" (λ "y" (λ "z" ("x" ∘ "z" ∘ ("y" ∘ "z"))))

b :: Expr
b = λ "x" (λ "y" (λ "z" ("x" ∘ ("y" ∘ "z"))))

c :: Expr
c = λ "x" (λ "y" (λ "z" ("x" ∘ "z" ∘ "y")))

w :: Expr
w = λ "x" (λ "y" ("x" ∘ "y" ∘ "y"))

ω :: Expr
ω = λ "x" ("x" ∘ "x")

-- Ω :: Expr
-- Ω = ω ∘ ω

y :: Expr
y = λ "g" (λ "x" ("g" ∘ ("x" ∘ "x")) ∘ λ "x" ("g" ∘ ("x" ∘ "x")))
