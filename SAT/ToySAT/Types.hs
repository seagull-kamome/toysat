{-# LANGUAGE ViewPatterns,OverloadedStrings,MagicHash #-}
module SAT.ToySAT.Types (CNF(..),Clause,Lit(..),neg,unLit,isPos) where

import Data.List (intersperse)
import Data.Function (on)

newtype Lit = Lit Int deriving(Eq)

unLit :: Lit -> Int
unLit (Lit x) = x

isPos :: Lit -> Bool
isPos (Lit x) = x >= 0

instance Ord Lit where
  compare x y = 
    case ((compare `on` (abs . unLit)) x y,x,y) of
      (EQ,isPos -> True, isPos -> False) -> GT
      (EQ,isPos -> False, isPos -> True) -> LT
      (EQ,_,_) -> EQ
      (c,_,_) -> c
instance Show Lit where
  show = show.unLit

neg :: Lit -> Lit
neg = Lit . negate . unLit

type Clause = [Lit]

--compareClause :: Clause -> Clause -> Ordering

data CNF = CNF Int [Clause]
instance Show CNF where
  show (CNF x ys) = 
    "p cnf " ++ (show x) ++ " " ++ (show $ length ys) ++ "\n"
    ++ (concatMap ((++ " 0\n").concat.intersperse " ".map show) ys)

