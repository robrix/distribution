{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Distribution where
import System.Random

data Expr a where
  StdRandom :: Random a => Expr a
  Lit :: a -> Expr a
