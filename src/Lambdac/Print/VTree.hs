{-# LANGUAGE TupleSections #-}

module Lambdac.Print.VTree where

import Lambdac.Print.Show ()
import Lambdac.Syntax (Expr (..))

data Glyph = VR Int | VV Int | UR Int deriving Show
type Glyphs = [Glyph]

type Frame = (Expr, Int)
type Stack = [Frame]

vtree :: Expr -> String
vtree e = go [] [(e,0)] []
 where
  go :: [String] -> Stack -> Glyphs -> String
  go ls [] _  = unlines . reverse $ ls
  go ls fs gs = let (l,fs',gs') = line fs gs
                 in go (l : ls) fs' gs'

  line :: Stack -> Glyphs -> (String, Stack, Glyphs)
  line (a:as) gs = let (s,as',gs') = draw a gs
                    in (s, as' ++ as, gs')

  draw :: Frame -> Glyphs -> (String, [Frame], Glyphs)
  draw (e,o) gs = if simp e
                     then (glyphs gs o ++ leaf e, [], next gs True)
                     else (glyphs gs o ++ root e, map (, o+2) (leaves e), next gs False ++ [VR o])

  next :: Glyphs -> Bool -> Glyphs
  next [] _ = []
  next gs isSimp
    | isSimp &&      isVR (last gs)  = init gs ++ [UR $ offset $ last gs]
    | isSimp && not (isVR (last gs)) = if null (init gs)
                                          then []
                                          else (init . init) gs ++ [UR $ offset . last . init $ gs]
    | not isSimp &&      isVR (last gs)  = init gs ++ [VV $ offset $ last gs]
    | not isSimp && not (isVR (last gs)) = init gs

  glyphs :: Glyphs -> Int -> String
  glyphs [] o = replicate o ' '
  glyphs gs o =
    let start = "\ESC[30m"
        end   = "\ESC[m"
        padding = replicate (o - offset (last gs) - 2) ' '
     in start ++ concat (zipWith draw' gs (VR (-2) : gs)) ++ padding ++ end

  draw' :: Glyph -> Glyph -> String
  draw' e s = replicate (offset e - offset s - 2) ' ' ++ glyph e

  simp :: Expr -> Bool
  simp (Var _) = True
  simp (Abs (Var _) (Var _)) = True
  simp (App (Var _) (Var _)) = True
  simp _ = False

  leaf :: Expr -> String
  leaf (Var x) = x
  leaf (Abs h b) = unwords ["λ", show h, show b]
  leaf (App f x) = unwords [show f, "@", show x]

  leaves :: Expr -> [Expr]
  leaves (Var x)   = []
  leaves (Abs h b) = [h, b]
  leaves (App f x) = [f, x]

  root :: Expr -> String
  root (Var x)   = x
  root (Abs _ _) = "λ"
  root (App _ _) = "@"

offset :: Glyph -> Int
offset (VR n) = n
offset (VV n) = n
offset (UR n) = n

glyph :: Glyph -> String
glyph (VR _) = "├ "
glyph (VV _) = "│ "
glyph (UR _) = "└ "

isVR :: Glyph -> Bool
isVR (VR _) = True
isVR _      = False
