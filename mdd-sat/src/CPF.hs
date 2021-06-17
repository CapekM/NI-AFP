module CPF where

import Data.MAPF
import Data.Graph
import Data.List
import Data.Maybe (fromJust)

-- | Creates MAPF_instance from loaded CPF file
create_mapf:: String -> MAPF_instance
create_mapf file =
   MAPF_instance (buildG (0, length edge_lines) (create_graph edge_lines [])) (create_agents vertex_lines vertex_lines [])
   where
      file_lines = delete_r $ lines file
      edge_lines = filter begins_edge file_lines
      vertex_lines = filter begins_vertex file_lines

-- | delete \r at the end
delete_r:: [String] -> [String]
delete_r (x:xs)
   | (last x) == '\r' = [init $ x] ++ delete_r xs
   | otherwise = x : delete_r xs
delete_r _ = []

-- | From vertex lines (first input) creates agent's start and goal destination
create_agents:: [String] -> [String] -> [(Int, Int)] -> [(Int, Int)]
create_agents (x:xs) v a
   | agent == 0 = create_agents xs v a
   | otherwise = create_agents xs v (a ++ [((agent_vertex x), (find_second agent v ))])
   where agent = agent_start x
create_agents _ _ n = n

-- | get first agent from vertex line
agent_start:: String -> Int
agent_start s = read $ fst $ splitAt (fromJust $ elemIndex ':' part) part :: Int
   where
      part = snd $ splitAt ((fromJust $ elemIndex '[' s) + 1) s

-- | find agents goal in vertex lines
find_second:: Int -> [String] -> Int
find_second a (x:xs)
   | a == agent = agent_vertex x
   | otherwise = find_second a xs
   where agent = agent_goal x

-- | get agents goal from vertex line
agent_goal:: String -> Int
agent_goal s = read $ init $ snd $ splitAt ((last $ findIndices (== ':') s) + 1) s :: Int

-- | get vertex from vertex line
agent_vertex:: String -> Int
agent_vertex s = read $ fst $ splitAt (fromJust $ elemIndex ':' ts) ts :: Int
   where ts = tail s

-- | From edge lines ::String creates edges ::Edge
create_graph:: [String] -> [Edge] -> [Edge]
create_graph (x:xs) g = create_graph xs (g ++ [((fst_vertex $ tail x), (snd_vertex $ tail x))])
create_graph [] g = g

-- | Returns first vertex of edge CPF line without first bracket
fst_vertex:: String -> Vertex
fst_vertex s = read $ fst $ splitAt (fromJust $ elemIndex ',' s) s :: Vertex

-- | Returns second vertex of edge CPF line without first bracket
snd_vertex:: String -> Vertex
snd_vertex s = read $ fst $ splitAt (fromJust $ elemIndex '}' part) part :: Vertex
   where
      part = snd $ splitAt ((fromJust $ elemIndex ',' s) + 1) s

-- | Check for vertex line in CPF format
begins_vertex (c:_) = c == '('
begins_vertex _ = False -- empty list

-- | Check for edge line in CPF format
begins_edge (c:_) = c == '{'
begins_edge _ = False -- empty list