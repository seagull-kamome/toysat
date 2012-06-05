{-# LANGUAGE ViewPatterns,OverloadedStrings,TypeFamilies,GADTs,RecordWildCards,NamedFieldPuns,ParallelListComp,FlexibleInstances #-}
module SAT.ToySAT.Monad (SAT, reserveLit, SATVar(..), ChoiseVar, IxVar, BoolVar, newLit) where

import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Data.Default
import Data.Ix (Ix,rangeSize)
import SAT.ToySAT.Types


type SAT m = StateT CNF (MaybeT m)

runSAT :: Monad m => SAT m a -> m (Maybe (a, CNF))
runSAT x = runMaybeT $ runStateT x def


genIntegralLit :: Monad m => Int -> SAT m Int
genIntegralLit n 
  | n < 1 = error "getIntegral : 'n' must greater then zero."
  | otherwise = do
    m <- get >>= (\(a,s) -> put s >> return a) . reserveLit n
    --m <- state $ reserveLit n  -- mtl-2.0.1.0ではstate関数はMonadStateのメソッドでは無いので通らない
    let cl | n == 1 = []
           | otherwise = concat $ [ [ [negate x, negate y] | y <- [(x+1)..(m+n-1) ]] | x <- [m..(m+n-2)] ]
    modify $ addClauses $ ([m..(m+n-1)]) : cl
    return m



newtype BoolVar = BoolVar Int
newLit :: Monad m => SAT m BoolVar
newLit = genIntegralLit 1 >>= return . BoolVar


data ChoiseVar a = ChoiseVar a Int
data IxVar a = Ix a => IxVar (a,a) Int

class SATVal a where
  type SATVar a
  newVar :: (Monad m) => a -> SAT m (SATVar a)
  --toLit :: (Monad m) => a -> (SATElem a) -> SAT m a
  
instance SATVal [a] where
  type SATVar [a] = ChoiseVar [a]
  newVar xs = genIntegralLit (length xs) >>= return . ChoiseVar xs

instance Ix a => SATVal (a,a) where
  type SATVar (a, a) = IxVar a
  newVar x = genIntegralLit (rangeSize x) >>= return . IxVar x

class SATVar a where
  assign :: SATElem a

test :: Monad m => SAT m ()
test = do
  a <- newVar [1,2,3,4]
  b <- newVar (1 :: Int, 9 :: Int) 
  return ()

