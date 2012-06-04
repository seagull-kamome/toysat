{-# LANGUAGE ViewPatterns,OverloadedStrings,MagicHash #-}
module SAT.ToySAT.Types (CNF(..),Clause,Lit(..),neg,unLit,isPos) where

import Control.Applicative ((<$>))
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

{- | 2つの節をマージします。

  1. 各々の節はソート済みで、かつ矛盾していない必要がある
  2. 互いに矛盾していたらNothingになります

>>> merge [Lit 1,Lit 2,Lit (-3)] [Lit 2,Lit 4,Lit 5,Lit 6]
Just [1,2,-3,4,5,6]

>>> merge [Lit 1,Lit 2,Lit 3] [Lit (-2),Lit 4,Lit 5,Lit 6]
Nothing

 -}
merge :: Clause -> Clause -> Maybe Clause
merge [] ys = Just ys
merge xs [] = Just xs
merge xss@(x:xs) yss@(y:ys)
  | x == neg y = Nothing
  | x == y     = (x:) <$> merge xs ys 
  | x < y      = (x:) <$> merge xs yss
  | otherwise  = (y:) <$> merge xss ys



data CNF = CNF Int [Clause]
instance Show CNF where
  show (CNF x ys) = 
    "p cnf " ++ (show x) ++ " " ++ (show $ length ys) ++ "\n"
    ++ (concatMap ((++ " 0\n").concat.intersperse " ".map show) ys)

