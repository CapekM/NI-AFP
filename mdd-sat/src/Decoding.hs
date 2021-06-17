module Decoding where

import Data.MAPF

import Data.Boolean.SatSolver

decode:: Mapping -> SatSolver -> [[Int]]
decode m ss = go_through (the_map m) ss m 0 []

go_through:: [Code_unit] -> SatSolver -> Mapping -> Int -> [[Int]] -> [[Int]]
go_through (x:xs) ss m prev a = case (lookupVar (encoded x) ss)of
    Just True  -> go_through xs ss m (agent x) (add_to_plan x m prev a)
    Just False -> go_through xs ss m prev a
    Nothing    -> go_through xs ss m prev a -- error "variable not assigned"
go_through _ _ _ _ a = a

add_to_plan:: Code_unit -> Mapping -> Int -> [[Int]] -> [[Int]]
add_to_plan x _ _ [] = [[(vertex x)]]
add_to_plan x m prev plan
    | prev == (agent x) = (init plan) ++ [(last plan) ++ [vertex x]]
    | otherwise = plan ++ [[(vertex x)]]
