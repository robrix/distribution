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
  Less :: Ord a => Expr a -> Expr a -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

data Var a where
  Double :: String -> Var Double
  Bool :: String -> Var Bool
  Int :: String -> Var Int

type Env = forall a. Var a -> [a]

lookupEnv :: Env -> Var a -> [a]
lookupEnv = id

emptyEnv :: Env
emptyEnv (Double v) = error ("unbound double variable " ++ v)
emptyEnv (Bool v) = error ("unbound bool variable " ++ v)
emptyEnv (Int v) = error ("unbound int variable " ++ v)

extendEnv :: Var a -> [a] -> Env -> Env
extendEnv (Double v) x _ (Double v') | v == v' = x
extendEnv (Bool v) x _ (Bool v') | v == v' = x
extendEnv (Int v) x _ (Int v') | v == v' = x
extendEnv _ _ env v' = env v'


instance Num a => Num (Expr a) where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = Sig
  fromInteger = Lit . fromInteger
  negate = Neg
