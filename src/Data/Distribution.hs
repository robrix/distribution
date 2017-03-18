{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes, StandaloneDeriving #-}
module Data.Distribution where

import Control.Applicative
import Control.Monad.Free.Freer
import Control.Monad.State
import Data.Functor.Classes
import Data.List (partition, sortOn)
import Data.Semigroup
import System.Random

data DistributionF a where
  StdRandom :: Random a => DistributionF a
  StdRandomR :: Random a => a -> a -> DistributionF a
  Get :: Var a -> DistributionF a
  Let :: Var a -> a -> a -> DistributionF a

data Var a where
  Double :: String -> Var Double
  Bool :: String -> Var Bool
  Int :: String -> Var Int

type Distribution = Freer DistributionF

newtype Env = Env { lookupEnv :: forall a. Var a -> a }

emptyEnv :: Env
emptyEnv = Env $ \ v -> case v of
  Double v -> error ("unbound double variable " ++ v)
  Bool   v -> error ("unbound bool variable " ++ v)
  Int    v -> error ("unbound int variable " ++ v)

extendEnv :: Var a -> a -> Env -> Env
extendEnv var x env = Env $ \ v -> case (var, v) of
  (Double v, Double v') | v == v' -> x
  (Bool v,   Bool v')   | v == v' -> x
  (Int v,    Int v')    | v == v' -> x
  _                               -> lookupEnv env v

sample :: Env -> Distribution a -> IO a
sample = flip (evalStateT . iterFreerA algebra)
  where algebra :: DistributionF x -> (x -> StateT Env IO a) -> StateT Env IO a
        algebra distribution cont = case distribution of
          StdRandom -> lift (getStdRandom random) >>= cont
          StdRandomR from to -> lift (getStdRandom (randomR (from, to))) >>= cont
          Get v -> do
            env <- get
            cont (lookupEnv env v)
          Let v e e' -> do
            modify (extendEnv v e)
            cont e'

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


if' :: Distribution Bool -> Distribution a -> Distribution a -> Distribution a
if' c a b = ifThenElse <$> c <*> a <*> b
  where ifThenElse c a b = if c then a else b


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

instance Show1 DistributionF where
  liftShowsPrec sp _ d dist = case dist of
    StdRandom -> showString "StdRandom"
    StdRandomR a b -> showsBinaryWith sp sp "StdRandomR" d a b
    Get a -> showsUnaryWith showsPrec "Get" d a
    Let var value body -> showsTernaryWith showsPrec sp sp "Let" d var value body
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
            showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

deriving instance Show (Var a)
