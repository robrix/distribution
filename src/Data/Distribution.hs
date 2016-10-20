{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Distribution where
import System.Random

data Expr a where
  StdRandom :: Random a => Expr a
  Lit :: a -> Expr a
  Get :: Var a -> Expr a
  Let :: Var a -> Expr a -> Expr b -> Expr b
  Not :: Expr Bool -> Expr Bool

data Var a where
  Double :: String -> Var Double
  Bool :: String -> Var Bool
  Int :: String -> Var Int
