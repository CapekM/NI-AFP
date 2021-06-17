# MI-AFP homework #04

Homework to practice types for text, containers, and work with errors

## Task

This time your task is more complex, you have some TODOs in three files and there is one more containing important data types for your implementation.

1. First *warm-up* task is to implement 7 very simple functions in `Data.Stack` (`src/Data/Stack.hs`). Do not change the data type definition and just implement functions typical for stack data structure (`pop`, `push`, `top`, `size`, `null`) and also `popSafe` + `topSafe` which use `Maybe` data type instead of throwing errors for empty stack.
2. After implementing `Data.Stack` you can proceed to implement `StackMachine`. Take a look at `Control.Program` where you find definitions of `Instruction`, `Program`, and other data types which you have to use (but do not change them). Then in `StackMachine` are prepared type synonyms which should be used in your implementation of `runProgram` as well. Stack machine takes a program with input and returns output (or throws an appropriate error).   
  You will need some helper function that includes stack, memory, and directory of labels (used for jumping). Implement logic of each instruction, their semantics are described in `Control.Program` and by tests in `test/StackMachineSpec.hs`. When all of them working correctly, you should be able to run more complex programs from `test/Fixtures/Programs.hs`.
3. Your last task is to implement *TODOs* in `Data.Logging`:
   * Implement three operators `$=` (change log level), `@@` (combine event sources), and `~~` (event source matching).
   * Use the `~~` operator and one powerful but simple higher-order function to implement `logFilter`. **Pointfree style is required!**

Hints & general requirements:

* Being [DRY](https://cs.wikipedia.org/wiki/Don%27t_repeat_yourself) is essential, do not repeat code. Name expressions to reuse then, introduce helper functions.
* Local names (via `where` or `let-in`) in functions should be introduced to make the code more readable. Creating helper functions in module scope is **awful**.
* For stack and stack machine, use pattern matching to the maximum! It is possible to implement both **without** using `if-then-else`. Recall that you can use "deeper" pattern matching (e.g., `foo (Maybe (x:xs))`) and that you should use wildcard `_` if you don't need the value.
* You can think about a program as a special list of instructions which you can process recursively and pass some context (I/O and internal state) along the way.
* For jump instruction, first construct the map with labels as keys and programs as values. This map you will then pass all the time without changing. There are no global variables! There is no mutability!
* In this homework you will work with various containers, look up the documentation. *Do not re-invent the wheel!*
* You must understand your code completely!

## Notes

 * In case of uncertainty, check the [dummy homework](https://github.com/MI-AFP/hw00) to recall what is the homework workflow for this course.
 * If you encounter some trouble, create an issue in your repository.
 * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
