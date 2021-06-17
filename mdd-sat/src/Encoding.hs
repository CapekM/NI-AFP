module Encoding where

import Data.MAPF
import MDD
import Makespan

import Data.Graph
import Data.List
import Data.Boolean.SatSolver

-- | Creates CNF formula for givven MAPF instance
create_encoding:: Int -> MAPF_instance -> (Boolean, Mapping)
create_encoding m i = (get_CNF 0 mdds (edges $ graph i) mapping, mapping)
    where
        mapping = create_mapping 0 mdds (Mapping [] 0)
        mdds = create_MDDs m (agents i) (edges $ graph i)

-- | Creates CNF formula from MDDs
get_CNF:: Int -> [[[Int]]] -> [Edge] -> Mapping -> Boolean
get_CNF a (x:xs) e m
    | null xs = formula
    | otherwise = formula :&&: get_CNF (a+1) xs e m
    where
        formula = disapearing :&&: edges_c
        disapearing = disallow_pairs a 0 x m
        edges_c = edge_constraints a 0 x e m

-- | Returns CNF formula representing edge constraint for given MDD
edge_constraints:: Int -> Int -> [[Int]] -> [Edge] -> Mapping -> Boolean
edge_constraints a t (x:xs:xss) e m
    | null xss = constraint
    | otherwise = constraint :&&: edge_constraints a (t+1) (xs:xss) e m
    where constraint = edge_constraint a t x xs e m
edge_constraints _ _ _ _ _ = Yes -- Special case with MDD with just one time expansion

-- | For two time-slices creates edge constraint
edge_constraint:: Int -> Int -> [Int] -> [Int] -> [Edge] -> Mapping -> Boolean
edge_constraint a t (x:xs) next e m
    | null xs = formula
    | otherwise = formula :&&: edge_constraint a t xs next e m
    where
        formula = (Not $ encode a t x m) :||: (disjunction a (t+1) neighbours m)
        neighbours = intersect ([x] ++ (get_neighbours x e)) next

-- | Create disjuction from list [x, y, z] to (x ∨ y ∨ z)
disjunction:: Int -> Int -> [Int] -> Mapping -> Boolean
disjunction a t (x:xs) m
    | null xs = encode a t x m
    | otherwise = (encode a t x m) :||: disjunction a t xs m
disjunction _ _ _ _ = Yes

-- | For every pair (x,y) in MDD time-slice (column) creates conjunction of (¬x ∨ ¬y)
disallow_pairs:: Int -> Int -> [[Int]] -> Mapping -> Boolean
disallow_pairs a t (x:xs) m
    | null xs = if single_el -- if the last list is one element list
        then encode a t (head x) m
        else formula
    | single_el = (encode a t (head x) m) :&&: disallow_pairs a (t+1) xs m -- no pairs to create
    | otherwise = formula :&&: disallow_pairs a (t+1) xs m
        where
            formula = list_to_boolean $ get_pairs a t x m
            single_el = (length x) == 1
disallow_pairs _ _ _ _ = error "should not be called with empty list"

-- | Does conjunction of clauses
list_to_boolean:: [Boolean] -> Boolean
list_to_boolean (x:xs) 
    | null xs = x
    | otherwise = x :&&: (list_to_boolean xs)
list_to_boolean _ = error "should not be called with empty list"

-- | Creates list of clauses
get_pairs:: Int -> Int -> [Int] -> Mapping -> [Boolean]
get_pairs a t l m = [((Not (encode a t x m)) :||: (Not (encode a t y m))) | (x:ys) <- tails (nub l), y <- ys]
