import Test.Hspec

import BinarySearchTree

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

mkLeaf   :: a -> BSTree a
mkLeaf x = Node x Nil Nil

mkLeft       :: a -> BSTree a -> BSTree a
mkLeft x mkLeft = Node x mkLeft Nil

-- valid trees
intTreeA = Node 7 (mkLeaf 5) (mkLeaf 8)       :: BSTree Int
intTreeB = Node 20 (mkLeaf 15) (mkLeaf 157)   :: BSTree Int
intTreeC = Node 10 intTreeA Nil               :: BSTree Int
intTreeD = Node 10 Nil intTreeB               :: BSTree Int
intTreeE = Node 10 intTreeA intTreeB          :: BSTree Int
-- invalid trees
intTreeF = Node 2 (mkLeaf 7) Nil              :: BSTree Int
intTreeG = Node 2 Nil (mkLeaf (-7))           :: BSTree Int
intTreeH = Node 10 intTreeB intTreeA          :: BSTree Int
intTreeI = Node 15 intTreeH intTreeH          :: BSTree Int
intTreeK = Node 5 (Node 2 Nil (mkLeaf 200)) (Node 10 (mkLeaf 0) Nil) :: BSTree Int
-- worst-case valid tree
intTreeJ = mkLeft 7 (mkLeft 5 (mkLeft 2 (mkLeaf (-7))))
-- after-insert trees
intTreeAi10 = Node 7 (mkLeaf 5) (Node 8 Nil (mkLeaf 10))
intTreeAi0 = Node 7 (Node 5 (mkLeaf 0) Nil) (mkLeaf 8)
intTreeCi45 = Node 10 intTreeA (mkLeaf 45)
intTreeDi8 = Node 10 (mkLeaf 8) intTreeB
intTreeEi0 = Node 10 intTreeAi0 intTreeB
intTreeFi7 = Node 2 (mkLeaf 7) (mkLeaf 7)
intTreeHi17 = Node 10 intTreeB (Node 7 (mkLeaf 5) (Node 8 Nil (mkLeaf 17)))
intTreeIi17 = Node 15 intTreeH intTreeHi17
intTreeJi3 = mkLeft 7 (mkLeft 5 (Node 2 (mkLeaf (-7)) (mkLeaf 3)))
-- after-delete trees
intTreeAd5 = Node 7 Nil (mkLeaf 8)
intTreeAd7 = Node 8 (mkLeaf 5) Nil
intTreeCd10 = intTreeA
intTreeCd7 = Node 10 intTreeAd7 Nil
intTreeDd10 = intTreeB
intTreeEd7 = Node 10 intTreeAd7 intTreeB
intTreeFd2 = mkLeaf 7
intTreeFd7 = intTreeF
intTreeHd10 = Node 5 intTreeB (Node 7 Nil (mkLeaf 8))
intTreeId10 = Node 15 intTreeHd10 intTreeH
intTreeJd5 = mkLeft 7 (mkLeft 2 (mkLeaf (-7)))

spec :: Spec
spec = do
  describe "isValid" $ do
    it "identifies correct trivial trees" $ do
      isValid (Nil :: BSTree Double) `shouldBe` True
      isValid (mkLeaf 3) `shouldBe` True
      isValid (mkLeaf "Hello") `shouldBe` True
    it "identifies correct complex trees" $ do
      isValid intTreeA `shouldBe` True
      isValid intTreeB `shouldBe` True
      isValid intTreeC `shouldBe` True
      isValid intTreeD `shouldBe` True
      isValid intTreeE `shouldBe` True
    it "identifies incorrect trees" $ do
      isValid intTreeF `shouldBe` False
      isValid intTreeG `shouldBe` False
      isValid intTreeH `shouldBe` False
      isValid intTreeK `shouldBe` False

  describe "isLeaf" $ do
    it "identifies leaf (sub)trees" $ do
      isLeaf (mkLeaf 3) `shouldBe` True
      isLeaf (mkLeaf "Hello") `shouldBe` True
    it "identifies non-leaf (sub)trees" $ do
      isLeaf (Nil :: BSTree Double) `shouldBe` False
      isLeaf intTreeC `shouldBe` False
      isLeaf intTreeD `shouldBe` False
      isLeaf intTreeE `shouldBe` False

  describe "size" $ do
    it "computes size of trivial tree (empty and mkLeaf)" $ do
      size (Nil :: BSTree Double) `shouldBe` 0
      size (mkLeaf 3) `shouldBe` 1
      size (mkLeaf "Hello") `shouldBe` 1
    it "computes size of non-trivial tree" $ do
      size intTreeA `shouldBe` 3
      size intTreeC `shouldBe` 4
      size intTreeE `shouldBe` 7
      size intTreeG `shouldBe` 2
      size intTreeJ `shouldBe` 4

  describe "height" $ do
    it "computes height of trivial tree (empty and mkLeaf)" $ do
      height (Nil :: BSTree Double) `shouldBe` 0
      height (mkLeaf 3) `shouldBe` 1
      height (mkLeaf "Hello") `shouldBe` 1
    it "computes height of non-trivial tree" $ do
      height intTreeA `shouldBe` 2
      height intTreeC `shouldBe` 3
      height intTreeE `shouldBe` 3
      height intTreeG `shouldBe` 2
      height intTreeJ `shouldBe` 4

  describe "minHeight" $ do
    it "computes minimal height in trivial tree (empty and mkLeaf)" $ do
      minHeight Nil `shouldBe` 0
      minHeight (mkLeaf 3) `shouldBe` 1
      minHeight (mkLeaf "Hello") `shouldBe` 1
    it "computes minimal height in non-trivial tree" $ do
      minHeight intTreeA `shouldBe` 2
      minHeight intTreeC `shouldBe` 1
      minHeight intTreeE `shouldBe` 3
      minHeight intTreeG `shouldBe` 1
      minHeight intTreeJ `shouldBe` 1

  describe "contains" $ do
    it "checks if element is in trivial tree (mixed)" $ do
      ((Nil :: BSTree Double) `contains` 7) `shouldBe` False
      contains (mkLeaf 3) 3 `shouldBe` True
      contains (mkLeaf 3) (-3) `shouldBe` False
      contains (mkLeaf "Hello") "Hello" `shouldBe` True
      contains (mkLeaf "Hello") "hello" `shouldBe` False
      contains (mkLeaf "Hello") "Hell" `shouldBe` False
    it "checks if element is in non-trivial tree (positive)" $ do
      (intTreeA `contains` 5) `shouldBe` True
      (intTreeA `contains` 7) `shouldBe` True
      (intTreeB `contains` 157) `shouldBe` True
      (intTreeC `contains` 10) `shouldBe` True
      (intTreeD `contains` 20) `shouldBe` True
      (intTreeE `contains` 157) `shouldBe` True
      (intTreeJ `contains` (-7)) `shouldBe` True
    it "checks if element is in non-trivial tree (negative)" $ do
      (intTreeA `contains` 9) `shouldBe` False
      (intTreeA `contains` 6) `shouldBe` False
      (intTreeB `contains` 100) `shouldBe` False
      (intTreeC `contains` 6) `shouldBe` False
      (intTreeD `contains` (-5)) `shouldBe` False
      (intTreeE `contains` 12) `shouldBe` False
      (intTreeJ `contains` 0) `shouldBe` False
    it "checks if element is in invalid tree (mixed, BS strategy)" $ do
      (intTreeF `contains` 2) `shouldBe` True
      (intTreeF `contains` 7) `shouldBe` False
      (intTreeG `contains` (-7)) `shouldBe` False
      (intTreeH `contains` 8) `shouldBe` False
      (intTreeH `contains` 20) `shouldBe` False
      (intTreeI `contains` 12) `shouldBe` False
      (intTreeI `contains` 10) `shouldBe` True
      (intTreeI `contains` 20) `shouldBe` False

  describe "insert" $ do
    it "inserts element to trivial tree" $ do
      (Nil `insert` 7) `shouldBe` mkLeaf 7
      (Nil `insert` "Hello") `shouldBe` Node "Hello" Nil Nil
    it "does not change tree if already contains element" $ do
      insert (mkLeaf 7) 7 `shouldBe` mkLeaf 7
      (intTreeA `insert` 7) `shouldBe` intTreeA
      (intTreeA `insert` 5) `shouldBe` intTreeA
      (intTreeA `insert` 8) `shouldBe` intTreeA
      (intTreeD `insert` 10) `shouldBe` intTreeD
      (intTreeD `insert` 157) `shouldBe` intTreeD
    it "inserts element to non-trivial tree" $ do
      (intTreeA `insert` 10) `shouldBe` intTreeAi10
      (intTreeA `insert` 0) `shouldBe` intTreeAi0
      (intTreeC `insert` 45) `shouldBe` intTreeCi45
      (intTreeD `insert` 8) `shouldBe` intTreeDi8
      (intTreeE `insert` 0) `shouldBe` intTreeEi0
      (intTreeJ `insert` 3) `shouldBe` intTreeJi3
    it "inserts element to invalid tree" $ do
      (intTreeF `insert` 7) `shouldBe` intTreeFi7
      (intTreeH `insert` 17) `shouldBe` intTreeHi17
      (intTreeI `insert` 17) `shouldBe` intTreeIi17

  describe "delete" $ do
    it "deletes element from trivial tree" $ do
      (mkLeaf 7 `delete` 7) `shouldBe` Nil
      (mkLeaf "Hello" `delete` "Hello") `shouldBe` Nil
    it "does not change tree if element in not there" $ do
      (intTreeA `delete` 15) `shouldBe` intTreeA
      (intTreeA `delete` 6) `shouldBe` intTreeA
      (intTreeA `delete` 0) `shouldBe` intTreeA
      (intTreeD `delete` 9) `shouldBe` intTreeD
      (intTreeD `delete` 150) `shouldBe` intTreeD
    it "deletes element from non-trivial tree" $ do
      (intTreeA `delete` 5) `shouldBe` intTreeAd5
      (intTreeA `delete` 7) `shouldBe` intTreeAd7
      (intTreeC `delete` 7) `shouldBe` intTreeCd7
      (intTreeC `delete` 10) `shouldBe` intTreeCd10
      (intTreeD `delete` 10) `shouldBe` intTreeDd10
      (intTreeE `delete` 7) `shouldBe` intTreeEd7
      (intTreeJ `delete` 5) `shouldBe` intTreeJd5
    it "deletes element from invalid tree" $ do
      (intTreeF `delete` 2) `shouldBe` intTreeFd2
      (intTreeF `delete` 7) `shouldBe` intTreeFd7
      (intTreeH `delete` 10) `shouldBe` intTreeHd10
      (intTreeI `delete` 10) `shouldBe` intTreeId10

  describe "toList" $ do
    it "converts trivial tree to list" $ do
      toList (Nil :: BSTree Double) `shouldBe` ([] :: [Double])
      toList (mkLeaf 5) `shouldBe` [5]
      toList (mkLeaf "Hello") `shouldBe` ["Hello"]
    it "converts non-trivial tree to list (sorted)" $ do
      toList intTreeA `shouldBe` [5, 7, 8]
      toList intTreeB `shouldBe` [15, 20, 157]
      toList intTreeC `shouldBe` [5, 7, 8, 10]
      toList intTreeD `shouldBe` [10, 15, 20, 157]
      toList intTreeE `shouldBe` [5, 7, 8, 10, 15, 20, 157]
      toList intTreeJ `shouldBe` [-7, 2, 5, 7]
    it "converts invalid tree to list (not sorted)" $ do
      toList intTreeF `shouldBe` [7, 2]
      toList intTreeG `shouldBe` [2, -7]
      toList intTreeH `shouldBe` [15, 20, 157, 10, 5, 7, 8]

  describe "fromList" $ do
    it "converts list to trivial tree" $ do
      fromList ([] :: [Double]) `shouldBe` (Nil :: BSTree Double)
      fromList [5] `shouldBe` mkLeaf 5
      fromList ["Hello"] `shouldBe` mkLeaf "Hello"
    it "converts list to balanced tree (trivial)" $ do
      fromList [5, 7, 8] `shouldBe` intTreeA
      fromList [7, 8, 5] `shouldBe` intTreeA
      fromList [7, 5, 8] `shouldBe` intTreeA
      fromList [8, 7, 5] `shouldBe` intTreeA
      fromList [8, 5, 7] `shouldBe` intTreeA
      fromList [5, 8, 7] `shouldBe` intTreeA
    it "does not create three with duplicities" $ do
      fromList [5, 5, 8] `shouldBe` Node 5 Nil (mkLeaf 8)
      fromList [5, 4, 5] `shouldBe` Node 4 Nil (mkLeaf 5)
    it "converts list to balanced tree (complex, odd)" $ do
      fromList [9, 0, 11, 15, 6] `shouldBe` Node 9 (Node 0 Nil (mkLeaf 6)) (Node 11 Nil (mkLeaf 15))
      fromList [2, 1, 4, 5, 3, 9, 15, 6, 7] `shouldBe` Node 5 (Node 2 (mkLeaf 1) (Node 3 Nil (mkLeaf 4))) (Node 7 (mkLeaf 6) (Node 9 Nil (mkLeaf 15)))
    it "converts list to balanced tree (complex, even)" $ do
      fromList [9, 0, 11, 15, 6, 8] `shouldBe` Node 8 (Node 0 Nil (mkLeaf 6)) (Node 11 (mkLeaf 9) (mkLeaf 15))
      fromList [2, 1, 4, 5, 3, 9, 15, 6] `shouldBe` Node 4 (Node 2 (mkLeaf 1) (mkLeaf 3)) (Node 6 (mkLeaf 5) (Node 9 Nil (mkLeaf 15)))
