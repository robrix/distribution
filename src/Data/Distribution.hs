{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Distribution where

import Control.Applicative
import Control.Monad
import Data.List (partition, sortOn)
import Data.Semigroup
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

sample :: Int -> Env -> Expr a -> IO [a]
sample n env expr
  | n > 0 = case expr of
    StdRandom -> traverse getStdRandom (replicate n random)
    Lit x -> pure (replicate n x)
    Get v -> pure (lookupEnv env v)
    Let v e e' -> do
      x <- sample' e
      sample n (extendEnv v x env) e'
    Not e -> fmap not <$> sample' e
    Neg e -> fmap negate <$> sample' e
    Abs e -> fmap abs <$> sample' e
    Sig e -> fmap signum <$> sample' e
    Exp e -> fmap exp <$> sample' e
    Log e -> fmap log <$> sample' e
    Add a b -> zipWith (+) <$> sample' a <*> sample' b
    Mul a b -> zipWith (*) <$> sample' a <*> sample' b

    Less a b -> zipWith (<) <$> sample' a <*> sample' b

    If c a b -> zipWith3 ifThenElse <$> sample' c <*> sample' a <*> sample' b

    Map f a -> fmap f <$> sample' a
    App f a b -> zipWith f <$> sample' a <*> sample' b
    Alt a b -> sample' a <|> sample' b
    Join a -> do
      a' <- sample' a
      join <$> traverse sample' a'
  | otherwise = pure []
  where sample' :: Expr a -> IO [a]
        sample' = sample n env
        ifThenElse c a b = if c then a else b

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
  fromInteger = Lit . fromInteger
  negate = Neg

instance Fractional a => Fractional (Expr a) where
  fromRational = Lit . fromRational
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
