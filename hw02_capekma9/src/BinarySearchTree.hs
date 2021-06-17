module BinarySearchTree where

import qualified Data.List
-- You might want to use some externals. Better use qualified import
-- so there won't be any clash
-- for example instead of "sort" (from Data.List) use "Data.List.sort"

-- !! DO NOT CHANGE BSTree data type and type signatures of functions

-- | Binary search tree as described at wikipedia:
--  https://en.wikipedia.org/wiki/Binary_search_tree
data BSTree a = Node a (BSTree a) (BSTree a)
              | Nil
              deriving (Show, Read, Eq)

value :: BSTree a -> a
value Nil = error "Nil does not have a value"
value (Node x _ _) = x

left :: BSTree a -> BSTree a
left Nil = Nil
left (Node _ l _) = l

right :: BSTree a -> BSTree a
right Nil = Nil
right (Node _ _ r) = r

-- | Check whether is @BSTree@ valid (i.e., does not violate any rule)
-- TODO: implement validity check
is_sorted :: Ord a => [a] -> Bool
is_sorted [] = True
is_sorted [x] = True
is_sorted (x:xs) = x <= head (xs) && is_sorted xs

isValid :: Ord a => BSTree a -> Bool
isValid = is_sorted . toList

-- does not check all values in subtree
-- isValid Nil = True
-- isValid (Node x Nil Nil) = True
-- isValid (Node x left Nil) = x >= value left && isValid left
-- isValid (Node x Nil right) = x <= value right && isValid right
-- isValid (Node x left right) = x >= value left && x <= value right && isValid left && isValid right

-- | Check whether is @BSTree@ is leaf
-- TODO: implement leaf check
isLeaf :: Ord a => BSTree a -> Bool
isLeaf x = (size x) == 1

-- | Count all nodes in @BSTree@
-- TODO: implement counting all nodes of the tree
size :: BSTree a -> Integer
size x = toInteger (length (toList x))

-- | Height of @BSTree@ (height of @Nil@ is 0)
-- TODO: implement finding out height of the tree
height :: BSTree a -> Integer
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- | Minimal height in the @BSTree@ (height of @Nil@ is 0)
-- TODO: implement finding out minimal depth of the tree
minHeight :: BSTree a -> Integer
minHeight Nil = 0
minHeight (Node _ left right) = 1 + min (minHeight left) (minHeight right)

-- | Check if given element is in the @BSTree@
-- TODO: implement finding out if element is in the tree
contains :: Ord a => BSTree a -> a -> Bool
contains Nil _ = False
contains (Node x left right) v
    | v == x = True
    | v < x = contains left v
    | v > x = contains right v

-- | Create new tree with given element inserted
-- TODO: implement insertion to the tree
insert :: Ord a => BSTree a -> a -> BSTree a
insert Nil x = Node x Nil Nil
insert (Node x left right ) v 
    | v == x = Node x left right
    | v < x = Node x (insert left v) right
    | v > x = Node x left (insert right v)

-- | Create new tree with given element deleted (min element in the right subtree strategy)
-- TODO: implement deletion from the tree
delete :: Ord a => BSTree a -> a -> BSTree a
-- delete x v = fromList $ filter (/=v) (toList x) -- does different deleting
delete Nil _ = Nil
delete (Node x left right) v
    | v == x = delete_him (Node x left right)
    | v < x = Node x (delete left v) right
    | v > x = Node x left (delete right v)

delete_him :: Ord a => BSTree a -> BSTree a
delete_him (Node x Nil Nil) = Nil
delete_him (Node x Nil right) = right
delete_him (Node x left Nil) = left
delete_him (Node x left right) = Node min_right_value left (delete right min_right_value)
    where min_right_value = min_value right

-- | Finds minimal value in given tree.
min_value :: Ord a => BSTree a -> a
min_value (Node x Nil _) = x
min_value (Node _ left _) = min_value left

-- | Convert @BSTree@ to list (will be in ascending order if tree is valid)
-- TODO: implement conversion from tree to list
toList :: BSTree a -> [a]
toList Nil = []
toList (Node x left right) = (toList left) ++ [x] ++ (toList right)

-- | Build new @BSTree@ from arbitrary list with use of median (left if even)
-- TODO: implement conversion from list to tree, use median (hint: sort)
fromList :: Ord a => [a] -> BSTree a
fromList [] = Nil
fromList l = create_tree $ split_by_median $ Data.List.sort $ Data.List.nub l

create_tree :: Ord a => ([a], [a]) -> BSTree a
create_tree (left, right) = Node (last left) (fromList $ init left) (fromList right)

split_by_median :: [a] -> ([a], [a])
split_by_median x = Data.List.splitAt median x
    where median = div ((length x) + 1) 2
