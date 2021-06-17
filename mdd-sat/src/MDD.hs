module MDD where

import Makespan
import Data.MAPF

import Data.Graph
import Data.List
import Data.Boolean.SatSolver

-- | Returns MDDs for given makespan and MAPF instance
create_MDDs:: Int -> [(Int, Int)] -> [Edge] -> [[[Int]]]
create_MDDs m (x:xs) e = [new_MDD] ++ create_MDDs m xs e
    where
        new_MDD = reverse_list $ create_MDD (reverse_list (create_TEG (m - 1) [(fst x)] e)) (create_TEG my_div [(snd x)] e)
        my_div = (fst qr) + (snd qr) -- division roud up
        qr = quotRem m 2
create_MDDs _ _ _ = []

-- | Returns TEGs for given makespan, vertex and graph
create_TEG:: Int -> [Int] -> [Edge] -> [[Int]]
create_TEG m from e
    | m == 1 = [new_layer]
    | otherwise = [new_layer] ++ create_TEG (m - 1) new_layer e
    where new_layer = time_expansion from e from

-- | Does one time expansion - returns input vertices + all neighbours of these vertices
time_expansion:: [Int] -> [Edge] -> [Int] -> [Int]
time_expansion (x:xs) e a = time_expansion xs e acc
    where acc = a ++ ((get_neighbours x e) \\ a)
time_expansion _ _ a = a

-- | Simply reverse list
reverse_list [] = []
reverse_list (x:xs) = reverse_list xs ++ [x]
