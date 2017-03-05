{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Distribution where

import Control.Applicative
import Control.Monad.Free.Freer
import Data.List (partition, sortOn)
import Data.Semigroup
import System.Random

data Distribution a where
  StdRandom :: Random a => Distribution a
  StdRandomR :: Random a => a -> a -> Distribution a
  Lit :: a -> Distribution a
  Get :: Var a -> Distribution a
  Let :: Var a -> Distribution a -> Distribution b -> Distribution b
  Not :: Distribution Bool -> Distribution Bool
  Neg :: Num a => Distribution a -> Distribution a
  Abs :: Num a => Distribution a -> Distribution a
  Sig :: Num a => Distribution a -> Distribution a
  Exp :: Floating a => Distribution a -> Distribution a
  Log :: Floating a => Distribution a -> Distribution a

  Add :: Num a => Distribution a -> Distribution a -> Distribution a
  Mul :: Num a => Distribution a -> Distribution a -> Distribution a
  Less :: Ord a => Distribution a -> Distribution a -> Distribution Bool
  If :: Distribution Bool -> Distribution a -> Distribution a -> Distribution a

  Map :: (b -> a) -> Distribution b -> Distribution a
  App :: (b -> c -> a) -> Distribution b -> Distribution c -> Distribution a
  Join :: Distribution (Distribution a) -> Distribution a
  Alt :: Distribution a -> Distribution a -> Distribution a

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

sample :: Env -> Distribution a -> IO a
sample env distribution = case distribution of
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
  where sample' :: Distribution a -> IO a
        sample' = sample env
        ifThenElse c a b = if c then a else b

samples :: Int -> Env -> Distribution a -> IO [a]
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

listOf :: Distribution a -> Distribution [a]
listOf element = do
  n <- abs <$> StdRandom :: Distribution Int
  listOfN (n `mod` 10) element

listOfN :: Int -> Distribution a -> Distribution [a]
listOfN n element | n > 0 = (:) <$> element <*> listOfN (pred n) element
                  | otherwise = pure []

frequency :: [(Int, Distribution a)] -> Distribution a
frequency [] = error "frequency called with empty list"
frequency choices = (`mod` total) . abs <$> (StdRandom :: Distribution Int) >>= pick sorted
  where total = sum (fst <$> sorted)
        sorted = reverse (sortOn fst choices)
        pick ((i, a) : rest) n
          | n <= i = a
          | otherwise = pick rest (n - i)
        pick _ _ = error "pick called with empty list"


unitDistribution :: (Fractional a, Random a) => Distribution a
unitDistribution = StdRandomR 0 1


-- Instances

instance Functor Distribution where
  fmap = Map

instance Applicative Distribution where
  pure = Lit
  (<*>) = App ($)

instance Monad Distribution where
  return = pure
  a >>= f = Join (fmap f a)

instance Semigroup (Distribution a) where
  (<>) = Alt

instance Monoid a => Monoid (Distribution a) where
  mempty = pure mempty
  mappend = (<>)

instance Num a => Num (Distribution a) where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = Sig
  fromInteger = pure . fromInteger
  negate = Neg

instance Fractional a => Fractional (Distribution a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Floating a => Floating (Distribution a) where
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

instance Bounded a => Bounded (Distribution a) where
  minBound = pure minBound
  maxBound = pure maxBound
