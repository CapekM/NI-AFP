module Data.MAPF where

import Data.Graph
import Data.List
import Data.Boolean.SatSolver

data MAPF_instance = MAPF_instance
   { graph     :: Graph
   , agents    :: [(Int, Int)]
   }
   deriving (Show, Eq, Read)

data Code_unit = Code_unit
   { agent     :: Int
   , time      :: Int
   , vertex    :: Int
   , encoded   :: Int
   }
   deriving (Show, Eq, Read)

-- not good for testing?
-- instance Eq Code_unit where
--     (==) x y = (agent t1) == (agent t2) && (time t1) == (time t2) && (vertex t1) == (vertex t2)

data Mapping = Mapping
   { the_map   :: [Code_unit]
   , size      :: Int
   }
   deriving (Show, Eq, Read)

-- | adds one element to Mapping
add:: Int -> Int -> Int -> Mapping -> Mapping
add a t v m = Mapping ((the_map m) ++ [Code_unit a t v (size m)]) new_size
   where
      new_size = (size m) + 1

-- | adds list of vertices to Mapping
add_list:: Int -> Int -> [Int] -> Mapping -> Mapping
add_list a t (v:vs) m = add_list a t vs new_map
   where
      new_map = add a t v m
add_list _ _ _ m = m

-- | encode [Agent, Time, Verticies] to propositional variable
encode:: Int -> Int -> Int -> Mapping -> Boolean
encode a t v m = case (find ( cond (Code_unit a t v 0)) (the_map m)) of
   Just v  -> Var $ encoded v
   Nothing -> error "can't find MDD vertex in mapping"
   where cond t1 t2 = (agent t1) == (agent t2) && (time t1) == (time t2) && (vertex t1) == (vertex t2)

get_agent:: Int -> Mapping -> Int
get_agent x m = case (find ( cond (Code_unit 0 0 0 x)) (the_map m)) of
   Just v  -> agent v
   Nothing -> error "can't find MDD vertex in mapping"
   where cond t1 t2 = (encoded t1) == (encoded t2)


-- | creates mapping  [vertex, time, agent] -> propositional variable
-- first Int represents agent and should start with 0
create_mapping:: Int -> [[[Int]]] -> Mapping -> Mapping
create_mapping a (x:xs) m = create_mapping (a + 1) xs new_map
    where
        new_map = encode_MDD a 0 x m
create_mapping _ _ a = a

-- | Adds [Agent, Time, Verticies] to mapping
encode_MDD:: Int -> Int -> [[Int]] -> Mapping -> Mapping
encode_MDD a t (x:xs) m = encode_MDD a (t + 1) xs new_map
    where
        new_map = add_list a t x m
encode_MDD _ _ _ a = a

-- | Returns 'reversed' MDD for 'reversed' TEG and half TEG 'from the goal side'
create_MDD:: [[Int]] -> [[Int]] -> [[Int]]
create_MDD (x:xs) (y:ys) = [(intersect x y)] ++ create_MDD xs ys
create_MDD teg _ = teg