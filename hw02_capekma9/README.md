# MI-AFP homework #02

Homework to practice using and creating simple functions as well as data types

## Task

Open `src/BinarySearchTree.hs` and implement all the TODOs there. You can also check the specification in `test/Spec.hs` and run the tests with `stack test`.

1. `isValid` should return `True` if given tree is sorted correctly, i.e., all values in left subtree are less than value of root and all values in right subtree are higher than value of root - for all subtrees. Otherwise it should return `False`.
2. `isLeaf` should return `True` if given (sub)tree is just a leaf (single value). Otherwise it should return `False`.
3. `size` should return number of value contained in given tree.
4. `height` should return (maximal) height of given tree. Height of leaf is 1.
5. `minHeight` should return minimal height of given tree.
6. `contains` should return `True` if given element if present in given tree (binary search), otherwise `False`.
7. `insert` should return new tree created by inserting given element to given tree. Insertion must follow rules of binary search tree (preserves validity). If element is already in tree, resulting tree is the same as given.
8. `delete` should return new tree created by deleting given element from given tree. If node with to-be-deleted element has both subtrees, it should be replaced by minimal value of right subtree (see [here](https://www.geeksforgeeks.org/binary-search-tree-set-2-delete/)). If element is not in three, resulting tree is the same as given.
9. `toList` should return a list with elements of given tree (prefix order). If tree is valid, then the list will be sorted in ascending order.
10. `fromList` should return a tree with elements from given list. Tree can not contain duplicates but list can. Tree should be balanced - use median strategy (if length of list is even, then left median). 

Hints: 

  * look at functions in `Prelude` & `Data.List` 
  * feel free to write your helper functions
  * prefer DRY, clearness and readability over efficiency
  * focus on using recursion and pattern matching
  * try to avoid conditions by pattern matching, but in some cases you might need to use `if then else`

## Notes 

  * If you encounter some trouble, create an issue in your repository.
  * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
