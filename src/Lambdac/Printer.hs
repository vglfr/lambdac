{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}

module Lambdac.Printer where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Lambdac.Syntax

{-

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
              ⡀ ⠄ ⠂ ⠁     ⠈ ⠐ ⠠ ⢀                -- 12
           @                       @
       ⡀ ⠁   ⠈ ⢀               ⡀ ⠁   ⠈ ⢀         -- 6
     @           @           @           @
   ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀       ⡀⠁ ⠈⢀     -- 3
  @     @     @     @     @     @     @     @
 / \   / \   / \   / \   / \   / \   / \   / \   -- 2
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
  = PNode a (PTree a) (PTree a)
  | PLeaf a
  deriving (Show, Foldable)

row :: Int -> PTree a -> [a]
row n t = go n [t]
 where
  go 0 ts = map root ts
  go n ts = go (n-1) (concatMap leaves ts)

root :: PTree a -> a
root (PNode v _ _) = v
root (PLeaf v)     = v

leaves :: PTree a -> [PTree a]
leaves (PNode _ l r) = [l, r]
leaves n             = [n]

left :: PTree a -> a
left (PNode _ l _) = root l
left (PLeaf _)     = error "leaf has no left branch"

-- right :: PTree a -> a
-- right (PNode _ _ r) = root r
-- right (PLeaf _)     = error "leaf has no right branch"

data Elem = Elem { tag :: String, off :: Int, width :: Int }

data Style = SPJ | Braille | HTree deriving Eq

fill :: Style -> Int -> String
fill f n
  | n == 0       = "  "
  | f == SPJ     = "/ \\"
  | f == Braille = fromJust $ lookup n [(2, "⡈ ⢁"), (3, "⡀⠁ ⠈⢀")] 
  | f == HTree   = let hh = replicate (n - 2) '─' 
                    in "┌" ++ hh ++ "┴" ++ hh ++ "┐"

class Tree a where
  tree :: a -> String

instance Tree Expr where
  tree = plot . build . ptree
   where
    ptree :: Expr -> PTree Elem
    ptree e = case e of
      Var v   -> PLeaf (Elem  v  0 0)
      Abs h b -> PNode (Elem "λ" 0 0) (ptree h) (ptree b)
      App f x -> PNode (Elem "@" 0 0) (ptree f) (ptree x)

    build :: PTree Elem -> PTree Elem
    build = go 0
     where
      go n t = if null $ row n t
                  then t
                  else let t' = modify n t
                        in if overlap n t'
                              then back n t'
                              else go (n+1) t'

      modify n t = let es = row n t
                    in undefined

               -- case t of
                 -- PLeaf e     -> PLeaf $ e { off = n }
                 -- PNode e l r -> undefined

      overlap n t = let os = off <$> row n t
                     in any (<2) $ zipWith (-) (tail os) os

      back 0 t = go 0 t
      back n t = let t' = undefined
                  in back (n-1) t'

    plot :: PTree Elem -> String
    plot t = let s = style t
                 o = offset t
              in unlines $ go s o 0
     where
      style t = let d = diff t
                 in if | d > 15 -> HTree
                       | d > 1  -> Braille
                       | otherwise -> SPJ

      diff t = off (root t) - off (left t)
      offset = negate . foldr (\x acc -> min acc (off x)) 0

      go s o n = let es = row n t
                  in if null es
                        then []                   -- "\ESC[30m" ++ ++ "\ESC[m"
                        else foldl' (draw o) "" es : foldl' (draw' s o) "" es : go s o (n+1)

      draw    o acc e = acc ++ replicate (o - off e - length acc - 1) ' ' ++ tag e
      draw' s o acc e = acc ++ replicate (o - off e - length acc - width e - 1) ' ' ++ fill s (width e)


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

