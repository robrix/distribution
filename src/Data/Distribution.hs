{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Distribution where

import Control.Applicative
import Data.List (partition, sortOn)
import Data.Semigroup
import System.Random

data Expr a where
  StdRandom :: Random a => Expr a
  StdRandomR :: Random a => a -> a -> Expr a
  Lit :: a -> Expr a
  Get :: Var a -> Expr a
  Let :: Var a -> Expr a -> Expr b -> Expr b
  Not :: Expr Bool -> Expr Bool
  Neg :: Num a => Expr a -> Expr a
  Abs :: Num a => Expr a -> Expr a
  Sig :: Num a => Expr a -> Expr a
  Exp :: Floating a => Expr a -> Expr a
  Log :: Floating a => Expr a -> Expr a

  Add :: Num a => Expr a -> Expr a -> Expr a
  Mul :: Num a => Expr a -> Expr a -> Expr a
  Less :: Ord a => Expr a -> Expr a -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

  Map :: (b -> a) -> Expr b -> Expr a
  App :: (b -> c -> a) -> Expr b -> Expr c -> Expr a
  Join :: Expr (Expr a) -> Expr a
  Alt :: Expr a -> Expr a -> Expr a

data Var a where
  Double :: String -> Var Double
  Bool :: String -> Var Bool
  Int :: String -> Var Int

type Env = forall a. Var a -> a

lookupEnv :: Env -> Var a -> a
lookupEnv = id

emptyEnv :: Env
emptyEnv (Double v) = error ("unbound double variable " ++ v)
emptyEnv (Bool v) = error ("unbound bool variable " ++ v)
emptyEnv (Int v) = error ("unbound int variable " ++ v)

extendEnv :: Var a -> a -> Env -> Env
extendEnv (Double v) x _ (Double v') | v == v' = x
extendEnv (Bool v) x _ (Bool v') | v == v' = x
extendEnv (Int v) x _ (Int v') | v == v' = x
extendEnv _ _ env v' = env v'

sample :: Env -> Expr a -> IO a
sample env expr = case expr of
  StdRandom -> getStdRandom random
  StdRandomR from to -> getStdRandom (randomR (from, to))
  Lit x -> pure x
  Get v -> pure (lookupEnv env v)
  Let v e e' -> do
    x <- sample' e
    sample (extendEnv v x env) e'
  Not e -> not <$> sample' e
  Neg e -> negate <$> sample' e
  Abs e -> abs <$> sample' e
  Sig e -> signum <$> sample' e
  Exp e -> exp <$> sample' e
  Log e -> log <$> sample' e
  Add a b -> (+) <$> sample' a <*> sample' b
  Mul a b -> (*) <$> sample' a <*> sample' b

  Less a b -> (<) <$> sample' a <*> sample' b

  If c a b -> ifThenElse <$> sample' c <*> sample' a <*> sample' b

  Map f a -> f <$> sample' a
  App f a b -> f <$> sample' a <*> sample' b
  Alt a b -> sample' a <|> sample' b
  Join a -> sample' a >>= sample'
  where sample' :: Expr a -> IO a
        sample' = sample env
        ifThenElse c a b = if c then a else b

samples :: Int -> Env -> Expr a -> IO [a]
samples n env = sequenceA . replicate n . sample env

histogramFrom :: Real a => a -> a -> [a] -> [Int]
histogramFrom from width samples
  | null samples = []
  | otherwise = length here : histogramFrom (from + width) width rest
  where (here, rest) = partition (<= from + width) samples

sparkify :: [Int] -> String
sparkify bins
  | null bins = ""
  | otherwise = spark <$> bins
  where sparks = " ▁▂▃▄▅▆▇█"
        maxSpark = pred (length sparks)
        max = maximum bins
        spark n = sparks !! round ((fromIntegral n * ((1.0 :: Double) / fromIntegral max)) * fromIntegral maxSpark)

listOf :: Expr a -> Expr [a]
listOf element = do
  n <- abs <$> StdRandom :: Expr Int
  listOfN (n `mod` 10) element

listOfN :: Int -> Expr a -> Expr [a]
listOfN n element | n > 0 = (:) <$> element <*> listOfN (pred n) element
                  | otherwise = pure []

frequency :: [(Int, Expr a)] -> Expr a
frequency [] = error "frequency called with empty list"
frequency choices = (`mod` total) . abs <$> (StdRandom :: Expr Int) >>= pick sorted
  where total = sum (fst <$> sorted)
        sorted = reverse (sortOn fst choices)
        pick ((i, a) : rest) n
          | n <= i = a
          | otherwise = pick rest (n - i)
        pick _ _ = error "pick called with empty list"


-- Instances

instance Functor Expr where
  fmap = Map

instance Applicative Expr where
  pure = Lit
  (<*>) = App ($)

instance Monad Expr where
  return = pure
  a >>= f = Join (fmap f a)

instance Semigroup (Expr a) where
  (<>) = Alt

instance Monoid a => Monoid (Expr a) where
  mempty = pure mempty
  mappend = (<>)

instance Num a => Num (Expr a) where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = Sig
  fromInteger = pure . fromInteger
  negate = Neg

instance Fractional a => Fractional (Expr a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Floating a => Floating (Expr a) where
  pi = Lit pi
  exp = Exp
  log = Log
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance Bounded a => Bounded (Expr a) where
  minBound = pure minBound
  maxBound = pure maxBound
