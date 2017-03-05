{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
module Data.Distribution where

import Control.Applicative
import Control.Monad.Free.Freer
import Data.List (partition, sortOn)
import Data.Semigroup
import System.Random

data DistributionF a where
  StdRandom :: Random a => DistributionF a
  StdRandomR :: Random a => a -> a -> DistributionF a
  Get :: Var a -> DistributionF a
  Let :: Var a -> Distribution a -> Distribution b -> DistributionF b
  Not :: Bool -> DistributionF Bool

  Less :: Ord a => a -> a -> DistributionF Bool
  If :: Bool -> a -> a -> DistributionF a

data Var a where
  Double :: String -> Var Double
  Bool :: String -> Var Bool
  Int :: String -> Var Int

type Distribution = Freer DistributionF

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
sample env = iterFreerA algebra
  where algebra :: DistributionF x -> (x -> IO a) -> IO a
        algebra distribution cont = case distribution of
          StdRandom -> getStdRandom random >>= cont
          StdRandomR from to -> getStdRandom (randomR (from, to)) >>= cont
          Get v -> cont (lookupEnv env v)
          Let v e e' -> do
            x <- sample env e
            sample (extendEnv v x env) e' >>= cont
          Not e -> cont (not e)

          Less a b -> cont $ a < b

          If c a b -> cont $ if c then a else b

samples :: Int -> Env -> Distribution a -> IO [a]
samples n env = sequenceA . replicate n . sample env


-- Inspection

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


-- Constructors

stdRandom :: Random a => Distribution a
stdRandom = StdRandom `Then` return

stdRandomR :: Random a => a -> a -> Distribution a
stdRandomR a b = StdRandomR a b `Then` return


listOf :: Distribution a -> Distribution [a]
listOf element = do
  n <- stdRandomR 0 10 :: Distribution Int
  listOfN n element

listOfN :: Int -> Distribution a -> Distribution [a]
listOfN n element | n > 0 = (:) <$> element <*> listOfN (pred n) element
                  | otherwise = pure []

frequency :: [(Int, Distribution a)] -> Distribution a
frequency [] = error "frequency called with empty list"
frequency choices = (stdRandomR 0 total :: Distribution Int) >>= pick sorted
  where total = sum (fst <$> sorted)
        sorted = reverse (sortOn fst choices)
        pick ((i, a) : rest) n
          | n <= i = a
          | otherwise = pick rest (n - i)
        pick _ _ = error "pick called with empty list"


unitDistribution :: (Fractional a, Random a) => Distribution a
unitDistribution = stdRandomR 0 1


-- Instances

instance Semigroup a => Semigroup (Distribution a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Distribution a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Num a => Num (Distribution a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

instance Fractional a => Fractional (Distribution a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Floating a => Floating (Distribution a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
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
