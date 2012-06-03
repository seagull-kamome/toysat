{-# LANGUAGE ViewPatterns,OverloadedStrings #-}
module SAT.ToySAT.Parser (cnfParser) where

import Control.Applicative ((<*>), (<*))
import Control.Monad (unless)
import qualified Data.Attoparsec.Char8 as AP

import SAT.ToySAT.Types

cnfParser :: AP.Parser CNF
cnfParser = 
  do { 
    AP.skipMany commentParser;
    AP.string "p cnf "; spc; n <- AP.decimal :: AP.Parser Int; spc; m <- AP.decimal; AP.endOfLine;
    cnf <- if m > 1 then AP.many1 (clauseParser n >>= return . filter ((/= 0).unL)) else return [];
    unless (length cnf == m) $ fail $ "Mismatch number of clauses. specified " ++ (show m) ++ ", But take " ++ (show $ length cnf);
    AP.endOfInput; 
    return $ CNF n cnf
    } AP.<?> "CNF"
  where
    spc = AP.skipWhile (flip elem " \t") -- skipSpaceは改行もスキップするので使えない?
    commentParser = do { AP.char 'c'; AP.skipWhile (/= '\n'); AP.endOfLine }
    clauseParser n = do { AP.many1 (litParser n) <* spc  <* AP.endOfLine } AP.<?> "Clause"
    litParser n = do { spc; AP.option P (AP.char '-' >> return N) <*> AP.decimal} AP.<?> "Literal"


