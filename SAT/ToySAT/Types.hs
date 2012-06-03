{-# LANGUAGE ViewPatterns,OverloadedStrings #-}
module SAT.ToySAT.Types (CNF(..),Clause,L(..),neg) where

import Data.List (intersperse)
import Data.Function (on)

data L = P { unL :: Int } | N { unL :: Int } deriving (Eq)
instance Ord L where
  compare x y = 
    case ((compare `on` unL) x y,x,y) of
      (EQ,P _,N _) -> GT
      (EQ,N _,P _) -> LT
      (EQ,_,_) -> EQ
      (c,_,_) -> c
instance Show L where
  show (P x) = show x
  show (N x) = show (-x)

neg :: L -> L
neg (P x) = N x
neg (N x) = P x

type Clause = [L]
data CNF = CNF Int [Clause]
instance Show CNF where
  show (CNF x ys) = 
    "p cnf " ++ (show x) ++ " " ++ (show $ length ys) ++ "\n"
    ++ (concatMap ((++ " 0\n").concat.intersperse " ".map show) ys)

