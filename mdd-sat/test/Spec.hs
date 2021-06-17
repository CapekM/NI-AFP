import Test.Hspec
import Data.Graph
import Data.Boolean.SatSolver

import Lib
import Data.MAPF
import CPF
import Makespan
import MDD
import Encoding
import Decoding

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

map_0 = Mapping [Code_unit 0 0 1 0, Code_unit 1 0 1 1] 2
map_A = Mapping [Code_unit 0 0 0 0, Code_unit 0 0 1 1, Code_unit 0 1 1 2, Code_unit 0 1 2 3] 4
map_B = Mapping [Code_unit 0 0 1 0, Code_unit 0 1 1 1, Code_unit 0 1 2 2, Code_unit 0 1 3 3] 4

spec :: Spec
spec = do

  describe "CPF module" $ do
    it "helper functions" $ do
      delete_r ["V =\r", "(1:-1)[0:0:0]\r"] `shouldBe` ["V =", "(1:-1)[0:0:0]"]
      delete_r ["V =", "(1:-1)[0:0:0]"] `shouldBe` ["V =", "(1:-1)[0:0:0]"]
      fst_vertex "0,1} (-1)" `shouldBe` 0
      snd_vertex "0,1} (-1)" `shouldBe` 1
      fst_vertex "10,1} (-1)" `shouldBe` 10
      snd_vertex "0,11} (-1)" `shouldBe` 11
      create_graph ["{0,1} (-1)","{1,2} (-1)","{1,3} (-1)"] [] `shouldBe` [(0,1), (1,2), (1,3)]
      agent_start "(0:-1)[1:2:2]" `shouldBe` 1
      agent_goal "(0:-1)[1:2:2]" `shouldBe` 2
      agent_start "(0:-1)[11:2:2]" `shouldBe` 11
      agent_goal "(0:-1)[1:22:22]" `shouldBe` 22
      agent_vertex "(0:-1)[1:22:22]" `shouldBe` 0
      agent_vertex "(10:-1)[1:22:22]" `shouldBe` 10
      create_agents ["(0:-1)[1:2:2]","(1:-1)[0:0:0]","(2:-1)[0:1:1]", "(3:-1)[2:0:0]"] ["(0:-1)[1:2:2]","(1:-1)[0:0:0]","(2:-1)[0:1:1]", "(3:-1)[2:0:0]"] [] `shouldBe` [(0,2), (3,0)]

    it "create MAPF instance" $ do
      create_mapf "V =\n(0:-1)[1:2:2]\n(1:-1)[0:0:0]\n(2:-1)[0:1:1]\n(3:-1)[2:0:0]\nE =\n{0,1} (-1)\n{1,2} (-1)\n{1,3} (-1)\n" `shouldBe` (MAPF_instance (buildG (0, 3) ([(0,1), (1,2), (1,3)])) [(0,2), (3,0)])

  describe "Makespan module" $ do
    it "functions" $ do
      get_neighbours 0 [(0,1), (1,2), (1,3)] `shouldBe` [1]
      get_neighbours 3 [(0,3), (1,2), (1,3)] `shouldBe` [0,1]
      get_path 3 0 [0] [] [(0,1), (1,2), (1,3)] `shouldBe` [0,1,2,3]
      get_path 1 0 [0] [] [(0,1), (1,2), (1,3)] `shouldBe` [0,1]
      get_shortest_path 3 [0,1,2] [0,1,2,3] [(0,1), (1,2), (1,3)] 0 `shouldBe` 2
      get_makespan (MAPF_instance (buildG (0, 3) ([(0,1), (1,2), (1,3)])) [(0,2), (3,0)]) `shouldBe` 2
      get_makespan (MAPF_instance (buildG (0, 8) [(0,3), (0,1), (1,4), (1,2), (3,6), (3,4), (4,7), (4,5), (2,5), (5,8), (6,7), (7,8)]) [(2,3), (1,4)]) `shouldBe` 3

  describe "Encoding module" $ do
    it "create nonecoded MDD" $ do
      create_TEG 2 [0] [(0,1), (1,2), (1,3)] `shouldBe` [[0,1],[0,1,2,3]] -- we do not include initial position
      create_TEG 2 [3] [(0,1), (1,2), (1,3)] `shouldBe` [[3,1],[3,1,0,2]]
      reverse_list (create_MDD [[0,1,2,3], [0,1]] [[2,1]]) `shouldBe` [[0,1],[1,2]]
      create_TEG 3 [2] [(0,3), (0,1), (1,4), (1,2), (3,6), (3,4), (4,7), (4,5), (2,5), (5,8), (6,7), (7,8)] `shouldBe` [[2,1,5],[2,1,5,0,4,8],[2,1,5,0,4,8,3,7]]
      create_TEG 2 [3] [(0,3), (0,1), (1,4), (1,2), (3,6), (3,4), (4,7), (4,5), (2,5), (5,8), (6,7), (7,8)] `shouldBe` [[3,0,6,4],[3,0,6,4,1,7,5]]
      reverse_list (create_MDD [[2,1,5,0,4,8,3,7], [2,1,5,0,4,8], [2,1,5]] [[3,0,6,4],[3,0,6,4,1,7,5]]) `shouldBe` [[2,1,5],[1,5,0,4],[0,4,3]]
      create_MDDs 2 [(0,2), (3,0)] [(0,1), (1,2), (1,3)] `shouldBe` [[[1]],[[1]]] -- we do not include initial position or gaol position
      create_MDDs 3 [(2,3), (1,4)] [(0,3), (0,1), (1,4), (1,2), (3,6), (3,4), (4,7), (4,5), (2,5), (5,8), (6,7), (7,8)] `shouldBe` [[[1,5],[0,4]],[[1,0,4,2],[1,4,3,7,5]]]

    it "create mapping" $ do
      create_mapping 0 [[[1]],[[1]]] (Mapping [] 0) `shouldBe` map_0
      create_mapping 0 [[[0,1],[1,2]]] (Mapping [] 0) `shouldBe` map_A
      create_mapping 0 [[[0,1]], [[1,2]]] (Mapping [] 0) `shouldBe` (Mapping [Code_unit 0 0 0 0, Code_unit 0 0 1 1, Code_unit 1 0 1 2, Code_unit 1 0 2 3] 4)

    it "creating CNF" $ do
      -- Boolean does not have comparsion thus function show:: Boolean -> String
      show (get_pairs 0 1 [1,2,3] map_B) `shouldBe` "[Not (Var 1) :||: Not (Var 2),Not (Var 1) :||: Not (Var 3),Not (Var 2) :||: Not (Var 3)]" -- [1,2,3] -> [(-1,-2), (-1,-3), (-2,-3)]
      show (disallow_pairs 0 0 [[1],[1,2,3]] map_B) `shouldBe` "Var 0 :&&: ((Not (Var 1) :||: Not (Var 2)) :&&: ((Not (Var 1) :||: Not (Var 3)) :&&: (Not (Var 2) :||: Not (Var 3))))"
      show (disallow_pairs 0 0 [[1]] map_B) `shouldBe` "Var 0"

      show (disjunction 0 1 [1,2,3] map_B) `shouldBe` "Var 1 :||: (Var 2 :||: Var 3)" -- todo unnecessary brackets
      show (edge_constraint 0 0 [1] [1,2,3] [(0,1), (1,2), (1,3)] map_B) `shouldBe` "Not (Var 0) :||: (Var 1 :||: (Var 2 :||: Var 3))"
      show (edge_constraints 0 0 [[1], [1,2,3]] [(0,1), (1,2), (1,3)] map_B) `shouldBe` "Not (Var 0) :||: (Var 1 :||: (Var 2 :||: Var 3))"
      -- show (edge_constraints [[2,1,5],[1,5,0,4],[0,4,3]] [(0,3), (0,1), (1,4), (1,2), (3,6), (3,4), (4,7), (4,5), (2,5), (5,8), (6,7), (7,8)]) `shouldBe` "((Not (Var 2) :||: (Var 1 :||: Var 5)) :&&: ((Not (Var 1) :||: (Var 1 :||: (Var 0 :||: Var 4))) :&&: (Not (Var 5) :||: (Var 5 :||: Var 4)))) :&&: ((Not (Var 1) :||: (Var 0 :||: Var 4)) :&&: ((Not (Var 5) :||: Var 4) :&&: ((Not (Var 0) :||: (Var 0 :||: Var 3)) :&&: (Not (Var 4) :||: (Var 4 :||: Var 3)))))"

      show (get_CNF 0 [[[1], [1,2,3]]] [(0,1), (1,2), (1,3)] map_B) `shouldBe` "(Var 0 :&&: ((Not (Var 1) :||: Not (Var 2)) :&&: ((Not (Var 1) :||: Not (Var 3)) :&&: (Not (Var 2) :||: Not (Var 3))))) :&&: (Not (Var 0) :||: (Var 1 :||: (Var 2 :||: Var 3)))"
      show (get_CNF 0 [[[1]],[[1]]] [(0,1), (1,2), (1,3)] map_0) `shouldBe` "(Var 0 :&&: Yes) :&&: (Var 1 :&&: Yes)"

    -- it "create encoding" $ do
    --   show (create_encoding 2 (MAPF_instance (buildG (0, 3) ([(0,1), (1,2), (1,3)])) [(0,2), (3,0)])) `shouldBe` "(((Not (Var 0) :||: Not (Var 1)) :&&: (Not (Var 2) :||: Not (Var 3))) :&&: ((Not (Var 0) :||: Var 2) :&&: (Not (Var 1) :||: (Var 2 :||: Var 3)))) :&&: (((Not (Var 4) :||: Not (Var 5)) :&&: (Not (Var 6) :||: Not (Var 7))) :&&: ((Not (Var 4) :||: Var 6) :&&: (Not (Var 5) :||: (Var 6 :||: Var 7))))"

  describe "Decoding module" $ do
    it "add_to_plan" $ do
      add_to_plan (Code_unit 0 0 0 0) map_A 0 [] `shouldBe` [[0]]
      add_to_plan (Code_unit 0 1 1 2) map_A 0 [[0]] `shouldBe` [[0,1]]
      add_to_plan (Code_unit 0 0 1 0) map_0 0 [] `shouldBe`  [[1]]
      add_to_plan (Code_unit 1 0 1 1) map_0 0 [[1]] `shouldBe`  [[1], [1]]

  describe "main" $ do
    it "mdd_sat" $ do
      -- map.cpf
      mdd_sat "V =\r\n(0:-1)[1:2:2]\r\n(1:-1)[0:0:0]\r\n(2:-1)[0:1:1]\r\n(3:-1)[2:0:0]\r\nE =\r\n{0,1} (-1)\r\n{1,2} (-1)\r\n{1,3} (-1)\r\n" `shouldBe` [[1], [1]]
      -- grid/grid_03x03_a2.cpf
      mdd_sat "V =\n(0:-1)[0:0:0]\n(1:-1)[2:0:0]\n(2:-1)[1:0:0]\n(3:-1)[0:1:1]\n(4:-1)[0:2:2]\n(5:-1)[0:0:0]\n(6:-1)[0:0:0]\n(7:-1)[0:0:0]\n(8:-1)[0:0:0]\nE =\n{0,3} (-1)\n{0,1} (-1)\n{1,4} (-1)\n{1,2} (-1)\n{3,6} (-1)\n{3,4} (-1)\n{4,7} (-1)\n{4,5} (-1)\n{2,5} (-1)\n{5,8} (-1)\n{6,7} (-1)\n{7,8} (-1)\n" `shouldBe` [[7],[5,4]]
      -- grid/grid_04x04_a2.cpf
      mdd_sat "V =\n(0:-1)[0:0:0]\n(1:-1)[0:0:0]\n(2:-1)[0:0:0]\n(3:-1)[0:0:0]\n(4:-1)[0:0:0]\n(5:-1)[0:1:1]\n(6:-1)[0:0:0]\n(7:-1)[1:0:0]\n(8:-1)[0:2:2]\n(9:-1)[0:0:0]\n(10:-1)[0:0:0]\n(11:-1)[0:0:0]\n(12:-1)[2:0:0]\n(13:-1)[0:0:0]\n(14:-1)[0:0:0]\n(15:-1)[0:0:0]\nE =\n{0,4} (-1)\n{0,1} (-1)\n{1,5} (-1)\n{1,2} (-1)\n{2,6} (-1)\n{2,3} (-1)\n{4,8} (-1)\n{4,5} (-1)\n{5,9} (-1)\n{5,6} (-1)\n{6,10} (-1)\n{6,7} (-1)\n{8,12} (-1)\n{8,9} (-1)\n{9,13} (-1)\n{9,10} (-1)\n{10,14} (-1)\n{10,11} (-1)\n{3,7} (-1)\n{7,11} (-1)\n{11,15} (-1)\n{12,13} (-1)\n{13,14} (-1)\n{14,15} (-1)\n" `shouldBe` [[6],[8]]

  --   it "main" $ do
  --     main "samples/map.cpf" `shouldBe` []
