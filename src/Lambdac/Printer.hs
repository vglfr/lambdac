{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Lambdac.Printer where

import Control.Arrow
import Lambdac.Syntax

{-
 | screen does not fit = ├ (vtree)
 | deltas more than 15 = ┴ (htree)
 | deltas more than 1  = ⡈ (braille)
 | otherwise           = / (spj)

-- tree  :: Tree -> String

           depth
             ↓
-- treeN :: Int -> Tree -> String
-- tree5 ::        Tree -> String
       ↑
      TH?

                LRRRLRL-like path to start
                            ↓
-- treeP :: Int -> Tree -> Path -> String

                       @
              ⡀ ⠄ ⠂ ⠁     ⠈ ⠐ ⠠ ⢀                -- 11
           @                       @
       ⡀ ⠁   ⠈ ⢀               ⡀ ⠁   ⠈ ⢀         -- 5
     @           @           @           @
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀     -- 2
  @     @     @     @     @     @     @     @
 / \   / \   / \   / \   / \   / \   / \   / \   -- 1
x   x y   y z   z u   u v   v w   w i   i j   j

  x    x     x      x
 /   ⡀⠁   ⡀⋅⠁   ⡀⠄⠂⠁
x   x    x     x

  x    x       x          x               x
 ⡈   ⡀⠁    ⡀⋅⠁      ⡀⠄⠂⠁        ⡀ ⠄ ⠂ ⠁
x   x    x       x          x

-}

{-

            λxy.xz y u
-> α [y'/y] λxy'.xz y u
-> β [y /x] λy'.yz u
-> β [y'/u] yz
            yz

-}

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

instance Repr Expr where
  repr (Var x)   = "(Var \"" ++ x ++ "\")"
  repr (Abs h b) = "(Abs " ++ repr h ++ " " ++ repr b ++ ")"
  repr (App f x) = "(App " ++ repr f ++ " " ++ repr x ++ ")"

data PTree a
  = PNode a (Tree a) (Tree a)
  | PLeaf a
  deriving Show

data Elem = Elem String Int
  deriving Functor

-- data Fill = SPJ | Braille | HTree

class Tree a where
  tree :: a -> String

instance Tree Expr where
  tree = splice . spine
  -- tree = spine >>> pick >>> join &&& fill >>> splice
   where
    spine :: Expr -> PTree Elem
    spine e = undefined
    -- spine e = go [Elem e 0]
     -- where
      -- go :: [Elem Expr] -> [[Elem String]]
      -- go [] = []
      -- go es = (fmap . fmap) root es : go (normalize . concatMap down $ es)

      -- down :: Elem Expr -> [Elem Expr]
      -- down (Elem (Var _) _) = []
      -- down (Elem e n) = [Elem (treel e) (n-2), Elem (treer e) (n+2)]

      -- normalize :: [Elem Expr] -> [Elem Expr]
      -- normalize [] = []
      -- normalize e:es = undefined

    pick :: PTree Elem -> (PTree Elem, Fill)
    pick es = (es, SPJ)

    join :: ([[Elem String]], Fill) -> [String]
    join _ = [""]

    fill :: ([[Elem String]], Fill) -> [String]
    fill _ = [""]

    splice :: ([String], [String]) -> String
    splice = unlines . fmap (uncurry (++)) . uncurry zip


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

treel :: Expr -> Expr
treel (Abs l _) = l
treel (App l _) = l

treer :: Expr -> Expr
treer (Abs _ r) = r
treer (App _ r) = r

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

