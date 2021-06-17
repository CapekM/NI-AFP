module Makespan where

import Data.MAPF
import Data.Graph
import Data.List
import Data.Maybe (fromJust)

-- | Returs initial makespan (maximum of shortest paths) 
get_makespan:: MAPF_instance -> Int
get_makespan i = maximum (get_paths (agents i) (edges $ graph i))

-- | Returns lenghts of agent's paths
get_paths:: [(Int, Int)] -> [Edge] -> [Int]
get_paths (x:xs) edges = [shortest_path] ++ get_paths xs edges
   where
      shortest_path = get_shortest_path (last path) (init path) path edges 0
      path = get_path (snd x) (fst x) [(fst x)] [] edges
get_paths _ _ = []

-- | Returns lenght of "path"
get_shortest_path:: Int -> [Int] -> [Int] -> [Edge] -> Int -> Int
get_shortest_path vertex (x:xs) path e a
   | is_neighbour vertex x e = get_shortest_path x new_path new_path e (a + 1)
   | otherwise = get_shortest_path vertex xs path e a
   where new_path = fst $ splitAt (fromJust (findIndex (== x) path)) path
get_shortest_path _ _ _ _ a = a

-- | Tells if two vertecies are neighbours in graph
is_neighbour:: Int -> Int -> [Edge] -> Bool
is_neighbour v x (e:es)
   | v == fst e && x == snd e = True 
   | v == snd e && x == fst e = True 
   | otherwise = is_neighbour v x es
is_neighbour _ _ _ = False

-- | Returns "path" (visited nodes from BFS)
get_path:: Int -> Int -> [Int] -> [Int] -> [Edge] -> [Int]
get_path to curr_vertex visited queue edges
   | curr_vertex == to = visited
   | otherwise = get_path to next_vertex (visited ++ [next_vertex]) (tail new_queue) edges
   where
      next_vertex = head new_queue
      new_queue = queue ++ ((get_neighbours curr_vertex edges) \\ visited)

-- | Returns neighbours of vertex in graph
get_neighbours:: Int -> [Edge] -> [Int]
get_neighbours from (x:xs)
   | from == fst x = [snd x] ++ get_neighbours from xs
   | from == snd x = [fst x] ++ get_neighbours from xs
   | otherwise = get_neighbours from xs
get_neighbours _ _ = []
