{-# LANGUAGE ViewPatterns,OverloadedStrings #-}
module SAT.ToySAT (CNF, solve, cnfParser) where

import SAT.ToySAT.Types
import SAT.ToySAT.Parser
import SAT.ToySAT.Solver

