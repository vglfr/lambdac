{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor (($>))
import Data.List ((\\), nub, singleton)
import Data.String (IsString (..))

data Expr
  = Var String
  | Abs Expr Expr
  | App Expr Expr
  deriving Eq

λ :: Expr -> Expr -> Expr
λ = Abs

(⋅) :: Expr -> Expr -> Expr
(⋅) = App

instance IsString Expr where
  fromString = Var

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

eval :: Expr -> IO Expr
eval e = do
  putStr "⋅ " >> print e
  case alpha e of
    Just e' -> eval e'
    Nothing -> case beta e of
                 Just e' -> if isAlpha e e'
                            then putStrLn "ω" >> putStrLn "" $> e
                            else eval e'
                 Nothing -> putStrLn "" $> e

alpha :: Expr -> Maybe Expr
alpha (App f x) = let f' = foldr rename f (free x)
                  in if f == f' then Nothing else Just (App f' x)
alpha _ = Nothing

beta :: Expr -> Maybe Expr
beta (Var _)   = Nothing
beta (Abs h b) = case b of
  e@(App _ _) -> beta e >>= Just . Abs h
  _           -> Nothing
beta (App f x) = case f of
  Var _       -> Nothing
  Abs h b     -> Just $ replace h x b
  e@(App _ _) -> beta e >>= (\y -> Just $ App y x)

isAlpha :: Expr -> Expr -> Bool
isAlpha e1 e2 = normalize e1 == normalize e2
 where
  normalize e = foldr renameDef e (zip (bound e) (map singleton ['a'..]))
  renameDef (f,t) v@(Var x) = if f == x then Var t else v
  renameDef ft    (Abs h b) = Abs (renameDef ft h) (renameDef ft b)
  renameDef ft    (App g x) = App (renameDef ft g) (renameDef ft x)

isBound :: String -> Expr -> Bool
isBound x e = x `elem` bound e

var :: Expr -> [String]
var (Var x)   = [x]
var (Abs h b) = nub $ show h : var b
var (App f x) = nub $ var f ++ var x

bound :: Expr -> [String]
bound (Var x)   = []
bound (Abs h b) = nub $ show h : bound b
bound (App f x) = nub $ bound f ++ bound x

free :: Expr -> [String]
free e = var e \\ bound e

rename :: String -> Expr -> Expr
rename x v@(Var y) = if x == y then Var (y ++ "'") else v
rename x (Abs h b) = Abs (rename x h) (rename x b)
rename x (App g y) = App (rename x g) (rename x y)

replace :: Expr -> Expr -> Expr -> Expr
replace f t v@(Var _) = if f == v then t else v
replace f t (Abs h b) = Abs h (replace f t b)
replace f t (App g x) = App (replace f t g) (replace f t x)

main :: IO Expr
main = do
  print $ λ "x" "x"
  putStrLn $ repr $ λ "x" "x"
  print $ λ "x" (λ "y" "x")
  putStrLn $ repr $ λ "x" (λ "y" "x")
  print $ λ "x" (λ "y" (λ "z" "x"))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" "x"))
  print $ λ "x" (λ "y" (λ "z" ("x" ⋅ "y" ⋅ "z")))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" ("x" ⋅ "y" ⋅ "z")))
  print $ λ "x" (λ "y" (λ "z" "x" ⋅ "y" ⋅ "z"))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" "x" ⋅ "y" ⋅ "z"))
  print $ λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" "v" ⋅ "z")))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" "v" ⋅ "z")))
  print $ λ "x" (λ "y" (λ "z" (λ "u" "v" ⋅ "y" ⋅ "z")))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" (λ "u" "v" ⋅ "y" ⋅ "z")))
  print $ λ "x" (λ "y" (λ "z" ("x" ⋅ "y" ⋅ λ "u" "v")))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" ("x" ⋅ "y" ⋅ λ "u" "v")))
  print $ λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" "v") ⋅ "z")))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" "v") ⋅ "z")))
  print $ λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" "v" ⋅ "w") ⋅ "z")))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" "v" ⋅ "w") ⋅ "z")))
  print $ λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" ("v" ⋅ "w")) ⋅ "z")))
  putStrLn $ repr $ λ "x" (λ "y" (λ "z" ("x" ⋅ λ "u" (λ "v" ("v" ⋅ "w")) ⋅ "z")))
  print $ λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "y" ⋅ "z")
  putStrLn $ repr $ λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "y" ⋅ "z")

  putStrLn ""

  eval "x"
  eval $ λ "x" "x" ⋅ "y"
  eval $ λ "x" "z" ⋅ "y"
  eval $ λ "x" (λ "y" "x") ⋅ "u"
  eval $ λ "x" (λ "y" "x") ⋅ "u" ⋅ "v"
  eval $ λ "x" (λ "y" "y") ⋅ "u"
  eval $ λ "x" (λ "y" "y") ⋅ "u" ⋅ "v"
  eval $ λ "x" (λ "y" "y") ⋅ "u" ⋅ "v" ⋅ "w"
  eval $ λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "v" ⋅ "z")
  eval $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")
  eval $ λ "x" ("x" ⋅ "x") ⋅ λ "y" ("y" ⋅ "y")
  eval $ λ "x" "x" ⋅ "x"
  eval $ λ "x" (λ "y" ("x" ⋅ "y")) ⋅ λ "z" (λ "c" "y" ⋅ "z")

  putStr "y -- " >> print (λ "x" "y")
  print $ isBound "y" (λ "x" "y")
  putStr "x -- " >> print (λ "x" "x")
  print $ isBound "x" (λ "x" "x")
  putStr "x -- " >> print (λ "x" (λ "y" "x") ⋅ "y")
  print $ isBound "x" (λ "x" (λ "y" "x") ⋅ "y")
  putStr "y -- " >> print (λ "x" (λ "y" "x") ⋅ "y")
  print $ isBound "y" (λ "x" (λ "y" "x") ⋅ "y")
  putStr "y -- " >> print (λ "x" (λ "z" "x") ⋅ "y")
  print $ isBound "y" (λ "x" (λ "z" "x") ⋅ "y")

  putStrLn ""

  putStr "x - " >> print (λ "x" (λ "z" "x"))
  print $ rename "x" (λ "x" (λ "y" "x"))

  putStrLn ""

  putStr (show $ λ "x" "x") >> putStr " " >> print (λ "x" "x")
  print $ isAlpha (λ "x" "x") (λ "x" "x")
  putStr (show $ λ "x" "x") >> putStr " " >> print (λ "x" "y")
  print $ isAlpha (λ "x" "x") (λ "x" "y")
  putStr (show $ λ "x" "y") >> putStr " " >> print (λ "z" "y")
  print $ isAlpha (λ "x" "y") (λ "z" "y")
  putStr (show $ λ "x" "z") >> putStr " " >> print (λ "z" "y")
  print $ isAlpha (λ "x" "z") (λ "z" "y")
  putStr (show $ λ "x" "x") >> putStr " " >> print (λ "y" "y")
  print $ isAlpha (λ "x" "x") (λ "y" "y")
  putStr (show $ λ "x" ("x" ⋅ "x")) >> putStr " " >> print (λ "y" ("y" ⋅ "y"))
  print $ isAlpha (λ "x" ("x" ⋅ "x")) (λ "y" ("y" ⋅ "y"))
  putStr (show $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")) >> putStr " " >> print (λ "x" ("x" ⋅ "x") ⋅ λ "y" ("y" ⋅ "y"))
  print $ isAlpha (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")) (λ "x" ("x" ⋅ "x") ⋅ λ "y" ("y" ⋅ "y"))

  print $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")
  print $ var (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x"))

  print $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")
  print $ bound (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x"))

  print $ λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x")
  print $ free (λ "x" ("x" ⋅ "x") ⋅ λ "x" ("x" ⋅ "x"))

  pure "x"
