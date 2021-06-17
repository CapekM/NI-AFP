module SortedListSpec (spec) where

import Data.Semigroup
import Test.Hspec
import Test.QuickCheck hiding (SortedList(..))

import Data.SortedList

empty :: SortedList Int
empty = Nil

list1 :: SortedList Int
list1 = fromList [1..5]

list2 :: SortedList Int
list2 = fromList [6,10]

list3 :: SortedList Int
list3 = fromList [1,2,3,4,5,6,10]

list4 :: SortedList Int
list4 = fromList [0,1,2,5,15]

badlist :: SortedList Int
badlist = 7 :<$ (2 :<$ Nil)


instance (Ord a, Arbitrary a) => Arbitrary (SortedList a) where
  arbitrary = fmap fromList rndlst
            where rndlst = sized $ \n -> do
                              k <- choose (0, n)
                              sequence [ arbitrary | _ <- [1..k] ]


prop_AssocInt :: SortedList Int ->  SortedList Int -> SortedList Int -> Bool
prop_AssocInt x y z = x <> (y <> z) == (x <> y) <> z

prop_NeutralInt :: SortedList Int -> Bool
prop_NeutralInt x = x <> mempty == x && mempty <> x == x

prop_FmapIdInt :: SortedList Int -> Bool
prop_FmapIdInt x = fmap id x == id x

prop_FmapComposeInt :: SortedList Int -> Bool
prop_FmapComposeInt x = fmap ((2*) . (\z -> z-5)) x == fmap (2*) (fmap (\z -> z-5) x)

spec :: Spec
spec = describe "data type SortedList" $ do
        it "is instance of Semigroup (<>)" $ do
          empty <> empty `shouldBe` empty
          list1 <> empty `shouldBe` list1
          empty <> list2 `shouldBe` list2
          list1 <> list2 `shouldBe` list3
          list2 <> list1 `shouldBe` list3
        it "is instance of Semigroup (and <> is associative)" $
          property prop_AssocInt
        it "is instance of Monoid (mempty, mappend, mconcat)" $ do
          mappend list1 mempty `shouldBe` list1
          mappend mempty list2 `shouldBe` list2
          mappend list1 list2 `shouldBe` list3
          mappend list2 list1 `shouldBe` list3
          mconcat [list2, list1, mempty] `shouldBe` list3
        it "is instance of Monoid (mempty is neutral)" $
          property prop_NeutralInt
        it "is instance of Functor (fmap, <$>)" $ do
          fmap (*2) list1 `shouldBe` fromList (map (*2) [1..5])
          fmap (1-) list1 `shouldBe` 0 :<$ ((-1) :<$ ((-2) :<$ ((-3) :<$ ((-4) :<$ Nil))))
          const 0 <$> list1 `shouldBe` fromList (replicate 5 0)
        it "is instance of Functor (fmap has identity)" $
          property prop_FmapIdInt
        it "is instance of Functor (fmap is homomorphic)" $
          property prop_FmapComposeInt
        it "is instance of Applicative (pure, <*>)" $ do
          pure 5 `shouldBe` fromList [5] -- cannot test function equality
          pure (*2) <*> list1 `shouldBe` fromList (map (*2) [1..5])
          pure (1-) <*> list1 `shouldBe` 0 :<$ ((-1) :<$ ((-2) :<$ ((-3) :<$ ((-4) :<$ Nil))))
          ((1-) :<$ pure (*3)) <*> list1 `shouldBe` 0 :<$ ((-1) :<$ ((-2) :<$ ((-3) :<$ ((-4) :<$ (3 :<$ (6 :<$ (9 :<$ (12 :<$ (15 :<$ Nil)))))))))
        it "is instance of Monad (>>)" $ do
          (empty >> empty) `shouldBe` empty
          (empty >> list1) `shouldBe` empty
          (list3 >> empty) `shouldBe` empty
          -- repeats like normal [] monad
          (list2 >> list1) `shouldBe` 1 :<$ (2 :<$ (3 :<$ (4 :<$ (5 :<$ (1 :<$ (2 :<$ (3 :<$ (4 :<$ (5 :<$ Nil)))))))))
        it "is instance of Monad (>>=)" $ do
          (empty >>= (\x -> return (x*x))) `shouldBe` empty
          (list1 >>= (\x -> return (x*x))) `shouldBe` fromList (map (^2) [1..5])
