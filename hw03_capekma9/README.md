# MI-AFP homework #03

Homework to practice different ways of branching in Haskell and modularization

## Task

Open `src/Lib.hs` and implement all the TODOs there. You can also check the specification in `test/Spec.hs` and run the tests with `stack test`.

To complete you will need to work also with `src/Examples.hs` and `src/Data/DummyList/Examples.hs`, but you are **not allowed** to change those nor data types in `src/Lib.hs` and it is also forbidden to add new dependencies.

1. `czechSalutation` should return salutation (in nominative, 1st case = "1. pád") in the Czech language for given person (gender, age, marital status and academic titles must be used appropriately). For details read the comment in code and check test specification, you can also read [(1)](https://www.muni.cz/o-univerzite/uredni-deska/oslovovani-akademickych-pracovniku), [(2)](http://www.etiketavse.estranky.cz/clanky/etiketa/4.-oslovovani-a-spolecenska-vyznamnost.html), and [(3)](http://www.studenta.cz/vysokoskolske-tituly-jak-oslovovat-na-akademicke-pude/magazin/article/587) if interested (Czech only). *Do not edit prepared data types, just implement the function.*
2. `intervalContains` should tell for given number if is in interval or not. *Do not edit prepared data type, just implement the function.*
3. `shapeCircumference` and `shapeArea` should return circumference and area of given shape (can be Circle, Square, Rectangle, or Triangle). *Do not edit prepared data type, just implement the function.*
4. `arithmeticSequence` should return for given `a` (first parameter) and `d` (second parameter) a [arithmetic sequence](https://en.wikipedia.org/wiki/Arithmetic_progression) as endless list.
5. `fibonacciNumbers` should return an endless list of [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number). Try to find a solution on your own and enjoy laziness with Haskell - it is so easy...
6. `matrixMultiplication` returns a product of two matrices `x` and `y` ([matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication), check the size). You must use list comprehension!
7. `dummyListExample1`, `stringExample2`, `stringExample2` should be assigned with values from `Data.DummyList.Examples` (`example1`) and `Data.MyString.Examples` (`example2` and `example3`). It is **not allowed** to copy or "implement" them, working import must be used.

Hints & general requirements:

* Being [DRY](https://cs.wikipedia.org/wiki/Don%27t_repeat_yourself) is essential, do not repeat code (for example, in inversed comparison of intervals).
* Local names (via `where` or `let-in`) in functions should be introduced to make the code more readable. Creating helper functions in module scope is **awful**.
* Avoid using `if-then-else` with patterns and guards (and/or combination of those two) if possible and better for readability.
* Look up functions that can help you (`Prelude`, `Data.List`) with general tasks like finding a maximum in list, converting `Integers` to generic numbers, or getting unique values from a list. *Do not re-invent the wheel!*
* You must understand your code completely!

## Notes

 * In case of uncertainty, check the [dummy homework](https://github.com/MI-AFP/hw00) to recall what is the homework workflow for this course.
 * If you encounter some trouble, create an issue in your repository.
 * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
