module Data.SortedList where

import Data.List (sort)
import Data.Semigroup

-- | List which is expected to be sorted
data SortedList a = a :<$ SortedList a
                  | Nil
                  deriving (Show, Read, Eq)

-- | Check validity of list (if is sorted)
-- DO NOT CHANGE THIS
valid :: Ord a => SortedList a -> Bool
valid Nil                   = True
valid (_ :<$ Nil)           = True
valid (x :<$ xs@(y :<$ ys)) = x < y && valid xs

-- | Construct sorted list from regular list
-- DO NOT CHANGE THIS
fromList :: Ord a => [a] -> SortedList a
fromList = foldr (:<$) Nil . sort

-- | Get the smallest element in sorted list
-- DO NOT CHANGE THIS
smallest :: Ord a => SortedList a -> a
smallest (x :<$ _) = x
smallest _ = error "SortedList: Nil"

instance Ord a => Semigroup (SortedList a) where
  -- | Merge two sorted lists
  -- DO NOT USE Data.List and its sort! it should be O(n+m) complexity
  (<>) = merge

instance Ord a => Monoid (SortedList a) where
  mempty = Nil
  mappend = (<>)

instance Functor SortedList where
  -- | Apply function over sorted list
  fmap _ Nil = Nil
  fmap f (x :<$ xs) = (f x) :<$ (fmap f xs)

instance Applicative SortedList where
  pure  = (:<$ Nil)
  -- | Apply all functions to elements in sorted list
  (<*>) Nil _ = Nil
  (<*>) (f :<$ fs) xs = fmap f xs `merge_2` (fs <*> xs)

instance Monad SortedList where
  -- | Apply on sorted list if valid and not empty
  (>>=) Nil _ = Nil
  (>>=) (x :<$ xs) f = f x `merge_2` (xs >>= f)


merge :: Ord a => SortedList a -> SortedList a -> SortedList a
merge Nil r = r
merge l Nil = l
merge l@(x :<$ xs) r@(y :<$ys) = case (x < y) of
  True -> x :<$ (merge xs r)
  _ -> y :<$ (merge l ys)

merge_2 :: SortedList a -> SortedList a -> SortedList a
merge_2 Nil r = r
merge_2 l Nil = l
merge_2 (l :<$ ls) r = l :<$ (ls `merge_2` r)