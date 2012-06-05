{-# LANGUAGE ViewPatterns,OverloadedStrings,MagicHash,RecordWildCards,NamedFieldPuns #-}
module SAT.ToySAT.Types (CNF(..),solved,Clause,Lit,compareLit,reserveLit,addClause,addClauses) where

import Control.Applicative ((<$>))
import Data.Default
import Data.List (intersperse)
import Data.Function (on)

type Lit = Int


{- |

>>> compareLit 1 1
EQ

>>> compareLit 1 2
LT

>>> compareLit 1 (-1)
GT

>>> compareLit (-1) 1
LT

>>> compareLit 1 (-2)
LT

 -}
compareLit :: Lit -> Lit -> Ordering
compareLit x y = 
  case ((compare `on` (abs)) x y, signum x, signum y) of
    (EQ,1, (-1)) -> GT
    (EQ,(-1), 1) -> LT
    (EQ,_,_) -> EQ
    (c,_,_) -> c



type Clause = [Lit]

--compareClause :: Clause -> Clause -> Ordering

{- | 2つの節をマージします。

  1. 各々の節はソート済みで、かつ矛盾していない必要がある
  2. 互いに矛盾していたらNothingになります

>>> merge [1, 2,(-3)] [2,4,5,6]
Just [1,2,-3,4,5,6]

>>> merge [1,2,3] [(-2),4,5,6]
Nothing

 -}
merge :: Clause -> Clause -> Maybe Clause
merge [] ys = Just ys
merge xs [] = Just xs
merge xss@(x:xs) yss@(y:ys)
  | x == negate y = Nothing
  | otherwise = case compareLit x y of
                  EQ -> (x:) <$> merge xs ys 
                  LT -> (x:) <$> merge xs yss
                  _  -> (y:) <$> merge xss ys



data CNF = CNF Int [Clause]
instance Show CNF where
  show (CNF x ys) = 
    "p cnf " ++ (show x) ++ " " ++ (show $ length ys) ++ "\n"
    ++ (concatMap ((++ " 0\n").concat.intersperse " ".map show) ys)
instance Default CNF where
  def = CNF 0 []


solved :: CNF -> Bool
solved (CNF _ ys) = null ys



reserveLit :: Int -> CNF -> (Int, CNF)
reserveLit n (CNF x ys) = (x + 1, CNF (x + n) ys)

addClause :: Clause -> CNF -> CNF
addClause cl (CNF x ys) = CNF x (cl:ys)

addClauses :: [Clause] -> CNF -> CNF
addClauses cl (CNF x ys) = CNF x (ys ++ cl)


