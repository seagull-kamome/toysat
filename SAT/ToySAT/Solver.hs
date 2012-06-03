{-# LANGUAGE ViewPatterns,OverloadedStrings #-}
module SAT.ToySAT.Solver (solve,resolve,cleanupRule,unitRule) where

import Data.List (sort,delete, partition,(\\), nub, intersect)
import Data.Maybe (mapMaybe)

import SAT.ToySAT.Types



{-| CNFを充足する解のリストを得ます 
  
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
                    ((l:_):_) -> concat $ map (\x -> maybe [] (go ans (newls ++ [x])) $ resolve x cnf) [l,neg l]
                    _ -> error "Internal error"
                  ) . unitRule


{- | 探索前にCNFをクリーンアップします

   クリーンアップとは、
     1. 同一の節にLが複数現れたら、1つにまとまる。(A ∨ A)はAと等しい
     2. 同一の節にLと¬Lの両方が現れたら、その節は除去する。(A ∨ ¬A) は常に1
     3. できるだけ早く刈り込みが進むように、節の大きさが小さい順に並べる
       a. 探索木の底の方で細かいバックトラックが何度も発生するのを回避できるかも
       b. 空になった節を早く発見する事ができるかも
       c. でも、どうせ処理中にひっくり返ってしまうので、意味ないと思う...

prop> \xs -> 1 == maximum (map (maximum.(map length).group.sort) $ cleanup xs)

-}
{-# INLINE cleanupRule #-}
cleanupRule :: [Clause] -> [Clause]
cleanupRule = mapMaybe (f [] . sort)
  where
    f zs [] = Just zs
    f zs [x] = Just (x:zs)
    f zs (x:xs@(y:_))
      | x == y     = f zs xs
      | x == neg y = Nothing
      | otherwise  = f (x:zs) xs

{- | 除去ルール

  (L∨A∨B∨C)∧(¬L∨D∨E∨F)は
  (A∨B∨C∨D∨E∨F)に置き換えできる。
-}



{- | 単一リテラル規則
   一つのリテラルLのみからなる節が存在した場合、Lは常に真で無くてはいけない
  という事なので、Lを含む節を節ごと除去し、¬Lを含む節はその節から¬Lを除去します。
  
  以下の場合は、節集合は充足不能なのでNothingを返します。
    1. Lのみからなる節と¬Lのみからなる節の両方を発見した場合
    2. 空の節ができたばあい
-}
{-# INLINE unitRule #-}
unitRule :: [Clause] -> Maybe ([Lit], [Clause])
unitRule xs
  | not $ null $ intersect ls ls' = Nothing
  | elem [] xs'' = Nothing
  | otherwise    = Just (ls, xs'')
  where
    (nub.concat -> ls, xs') = partition (null.tail) xs
    ls' = map neg ls
    xs'' = map (\\ ls') $ filter (null.intersect ls) xs'


{- | リテラルが真であるとみなして、節集合を簡略化します
   1. Lを含む節を除去します
   2. ¬Lを除去します
   3.空の節ができた場合はNothingを戻します
-}
resolve :: Lit -> [Clause] -> Maybe [Clause]
resolve l xs 
  | elem [] newclauses = Nothing
  | otherwise = Just newclauses
  where 
    newclauses = map (delete (neg l)) $ filter (not.elem l) xs



