{-# LANGUAGE ViewPatterns,OverloadedStrings #-}
module SAT.ToySAT.Solver (solve,step,eval,cleanupRule,unitRule) where

import Data.List (sortBy,delete, partition,(\\), nub, intersect)
import Data.Maybe (mapMaybe)

import SAT.ToySAT.Types



{-| CNFを充足する解のリストを得ます 

  かならずしも全ての解を検出できるとは限らない。

要テスト: 解が見つかった端から取り出せるようにする事。最後まで見つからないと出てこないのはNG。
-}
solve :: CNF -> [[Lit]]
solve = go [] [] . cleanupRule . (\(CNF _ x) -> x)
  where 
    go ans ls = maybe ans 
                (\(ls', cnf) ->
                  let newls = ls ++ ls'
                  in case cnf of
                    ([]) -> ans ++ [newls]
                    ((l:_):_) -> concat $ map (\x -> maybe [] (go ans (newls ++ x)) $ eval x cnf) [[l],[negate l]]
                    _ -> error "Internal error"
                  ) . unitRule

{- |
 -}
step :: [Lit] -> CNF -> Maybe ([Lit],CNF)
step ls (CNF x ys) = do
  (ls', ys') <- eval ls ys >>= unitRule
  return $ (ls', CNF x ys')




{- | 探索前にCNFをクリーンアップします

   クリーンアップとは、
     1. 同一の節にLが複数現れたら、1つにまとまる。(A ∨ A)はAと等しい
     2. 同一の節にLと￢Lの両方が現れたら、その節は除去する。(A ∨ ￢A) は常に1

prop> \xs -> 1 == maximum (map (maximum.(map length).group.sortBy compareLit) $ cleanup xs)

-}
{-# INLINE cleanupRule #-}
cleanupRule :: [Clause] -> [Clause]
cleanupRule = mapMaybe (f [] . sortBy compareLit)
  where
    f zs [] = Just zs
    f zs [x] = Just (x:zs)
    f zs (x:xs@(y:_))
      | x == y        = f zs xs
      | x == negate y = Nothing
      | otherwise     = f (x:zs) xs



{- | 導出ルール (L∨A)∧(￢L∨B) -> (A∨B)
-}
--resolve :: Clause -> Clause -> Maybe Clause

{- | 単一リテラル規則
   一つのリテラルLのみからなる節が存在した場合、Lは常に真で無くてはいけない
  という事なので、Lを含む節を節ごと除去し、￢Lを含む節はその節から￢Lを除去します。
  
  以下の場合は、節集合は充足不能なのでNothingを返します。
    1. Lのみからなる節と￢Lのみからなる節の両方を発見した場合
    2. 空の節ができたばあい
-}
{-# INLINE unitRule #-}
unitRule :: [Clause] -> Maybe ([Lit], [Clause])
unitRule xs
  | not $ null $ intersect ls ls' = Nothing
  | elem [] xs'' = Nothing
  | null ls      = return (ls, xs'')
  | otherwise    = do
    (x, y) <- unitRule xs''
    return (x ++ ls, y)
  where
    (nub.concat -> ls, xs') = partition (null.tail) xs
    ls' = map negate ls
    xs'' = map (\\ ls') $ filter (null.intersect ls) xs'


{- | リテラルが真であるとみなして、節集合を簡略化します
   1. Lを含む節を除去します
   2. ￢Lを除去します
   3.空の節ができた場合はNothingを戻します
-}
eval :: [Lit] -> [Clause] -> Maybe [Clause]
eval ls xs 
  | elem [] newclauses = Nothing
  | otherwise = Just newclauses
  where 
    ls' = map negate ls
    newclauses = map (\\ ls') $ filter (null.intersect ls) xs


