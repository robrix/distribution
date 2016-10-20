{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Distribution where
import System.Random

data Expr a where
  StdRandom :: Random a => Expr a
  Lit :: a -> Expr a
  Get :: Var a -> Expr a
  Let :: Var a -> Expr a -> Expr b -> Expr b
  Not :: Expr Bool -> Expr Bool
  Neg :: Num a => Expr a -> Expr a
  Abs :: Num a => Expr a -> Expr a
  Sig :: Num a => Expr a -> Expr a
  Add :: Num a => Expr a -> Expr a -> Expr a
  Mul :: Num a => Expr a -> Expr a -> Expr a

data Var a where
  Double :: String -> Var Double
  Bool :: String -> Var Bool
  Int :: String -> Var Int


instance Num a => Num (Expr a) where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = Sig
  fromInteger = Lit . fromInteger
  negate = Neg
