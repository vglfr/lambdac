{-# LANGUAGE OverloadedStrings #-}

module Lambdac.Printer where

import Lambdac.Syntax

instance Show Expr where
  show (Var x)   = x
  show (Abs h b) = "λ" ++ show h ++ showAbs b
   where
    showAbs (Abs h' b') = show h' ++ showAbs b'
    showAbs e           = "." ++ showInner e
    showInner e@(Abs _ _) = "(" ++ show e ++ ")"
    showInner (App f x)   = showInner f ++ showInner x
    showInner e           = show e
  show (App f x) = show f ++ " " ++ show x

class Repr a where
  repr :: a -> String
  -- tree :: a -> String

instance Repr Expr where
  repr (Var x)   = "(Var \"" ++ x ++ "\")"
  repr (Abs h b) = "(Abs " ++ repr h ++ " " ++ repr b ++ ")"
  repr (App f x) = "(App " ++ repr f ++ " " ++ repr x ++ ")"
  -- tree (Var x)   = x
  -- tree (Abs h b) = undefined

{-

λx.x(λz.z) λy.(λz.z)y

∙
├ λ
│ ├ x
│ └ ∙
│   ├ x
│   └ λ z z
└ λ
  ├ y
  └ ∙
    ├ λ z z
    └ y

 -}

--           ├        │         -- └ on pop from Glyphs
data Glyph = VR Int | VV Int deriving Show
type Glyphs = [Glyph]

expr' :: Expr
expr' = λ "x" (λ "y" "x")
-- expr' = λ "x" "x"
-- expr' = "x"
-- expr' = λ "x" ("x" ⋅ λ "z" "z") ⋅ λ "y" (λ "z" "z" ⋅ "y")

type Frame = (Expr, Int)
type Stack = [Frame]

tree :: Expr -> String
-- tree = unlines . go [] . pure . (,0)
-- tree e = unlines . go [] $ [(e,0)]
tree e = go [] [(e,0)] []
 where
  go :: [String] -> Stack -> Glyphs -> String
  go ls [] _  = unlines . reverse $ ls
  go ls fs gs = let (l,fs',gs') = line fs gs
                 in go (l : ls) fs' gs'

line :: Stack -> Glyphs -> (String, Stack, Glyphs)
line (a:as) gs = let (s,as',gs') = f a gs
                  in (s, as' ++ as, gs')
 where
  f :: Frame -> Glyphs -> (String, [Frame], Glyphs)
  f (e,o) gs = if simp e
                  then (skeleton ur o ++ leaf e, [], gs) -- └
                  else (skeleton vr o ++ root e, map (\x -> (x,o+2)) (leaves e), gs ++ [VR o]) -- ├

skeleton :: String -> Int -> String
skeleton cs n =
  if n == 0
     then replicate n ' '
     else replicate (n-2) ' ' ++ cs

vv :: String
vv = "│ "

vr :: String
vr = "├ "

ur :: String
ur = "└ "

sh :: String
sh = "\ESC[30m"

eh :: String
eh = "\ESC[m"

offset :: Glyph -> Int
offset (VR n) = n
offset (VV n) = n

glyphs' :: Glyphs
-- glyphs' = [VR 0]
-- glyphs' = [VV 0, VR 2]
glyphs' = [VV 0, VV 4, VV 8, VR 10]

glyphs :: Glyphs -> Int -> String
glyphs (g:gs) o = concat $ drawHead g : map drawTail (zip gs (g:gs)) ++ [offset']
 where
  drawHead :: Glyph -> String
  drawHead (VR n) = replicate n ' ' ++ vr
  drawHead (VV n) = replicate n ' ' ++ vv

  drawTail :: (Glyph, Glyph) -> String
  drawTail (VR e, s) = replicate (e - offset s - 2) ' ' ++ vr
  drawTail (VV e, s) = replicate (e - offset s - 2) ' ' ++ vv

  offset' :: String
  offset' = if null gs
               then ""
               else replicate (o - offset (last gs) - 2) ' '

simp :: Expr -> Bool
simp (Var _) = True
simp (Abs (Var _) (Var _)) = True
simp (App (Var _) (Var _)) = True
simp _ = False

leaf :: Expr -> String
leaf (Var x) = x
leaf (Abs h b) = unwords ["λ", show h, show b]
leaf (App f x) = unwords [show f, "∙", show x]

root :: Expr -> String
root (Var x)   = x
root (Abs _ _) = "λ"
root (App _ _) = "∙"

leaves :: Expr -> [Expr]
leaves (Var x)   = []
leaves (Abs h b) = [h, b]
leaves (App f x) = [f, x]
