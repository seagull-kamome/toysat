{-# LANGUAGE ViewPatterns,OverloadedStrings,TypeFamilies,GADTs,RecordWildCards,NamedFieldPuns #-}
module SAT.ToySAT.Monad (SAT, reserveLit, SATVar(..), ChoiseVar, IxVar, BoolVar, newLit) where

import Control.Monad.Trans.State
import Data.Ix (Ix,rangeSize)
import SAT.ToySAT.Types


data Probrem =  Probrem {
  nextLit :: Int,
  clause :: [Clause]
  }


type SAT m = StateT Probrem m
  
reserveLit :: Monad m => Int -> SAT m Int
reserveLit n = state (\s@(Probrem {..}) -> (nextLit, s { nextLit = nextLit + n }))



class SATVar a where
  type SATVal a
  type SATElem a
  newVar :: (Monad m) =>　(SATVal a) -> SAT m a
  --toLit :: (Monad m) => a -> (SATElem a) -> SAT m a
  


data ChoiseVar a = ChoiseVar [a] Int
instance SATVar (ChoiseVar a) where
  type SATVal (ChoiseVar a) = [a]
  type SATElem (ChoiseVar a) = a
  newVar xs = reserveLit n >>= return . ChoiseVar xs
    where n = length xs


data IxVar a = Ix a => IxVar (a,a) Int
instance Ix a => SATVar (IxVar a) where
  type SATVal (IxVar a) = (a, a)
  type SATElem (IxVar a) = a
  newVar x = reserveLit (rangeSize x) >>= return . IxVar x


newtype BoolVar = BoolVar Int
newLit :: Monad m => SAT m BoolVar
newLit = reserveLit 1 >>= return . BoolVar




--(:->) :: 



myprobrem = do
  apples <- newVar (0,20)
  oranges <- newVar (0,50)
  (apples :*: 50) :+: (oranges :*: 30) :<=: 2000
  with apples `must` ((`mod` 2) == 0)
  (apples :>=: 3) :->: (with oranges  `must` (>= 5)

