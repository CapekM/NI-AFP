module Lib where

import Data.MAPF
import CPF
import Makespan
import Encoding
import Decoding

import Data.Boolean.SatSolver
import Data.Graph
import Data.Maybe (fromJust)
import Data.List

mdd_sat:: String -> [[Int]]
mdd_sat file =
   mdd_solve (get_makespan inst) inst
   where
      inst = create_mapf file

-- | main loop that encodes problem to CNF and tries to solve it
-- now it just returns makespan
mdd_solve:: Int -> MAPF_instance -> [[Int]]
mdd_solve m i =
   case (assertTrue cnf newSatSolver) of
      Just ss -> case (solve ss) of
         Just solved_ss -> decode mapping solved_ss
            -- isSolved == False ->  error "this cannot happen without collision constraint" -- mdd_solve (m+1) i
         Nothing -> error "canot solve"
      Nothing -> error "could not create SatSolver"
   where
      cnf = fst encoding
      mapping = snd encoding
      encoding = create_encoding m i

-- call_mdd_sat:: String -> IO Int
-- call_mdd_sat filename = do
--   template <- loadFileMay $ filename
--   case template of
--     Nothing -> return (-1)
--     Just v -> return (mdd_sat v)
