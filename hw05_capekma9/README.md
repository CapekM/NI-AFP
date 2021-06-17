# MI-AFP homework #05

Homework to practice advanced work with typeclasses

## Task

1. You've already done some implementation of `area` and `circumference` for various shapes in the [third homework](https://github.com/MI-AFP/hw03). Now you will do something similar, but using typeclasses in `Data.Shapes`!
   * First, implement instances of  `Validable` for each shape type with function `valid` that says if given shape is valid (its attributes are acceptable, i.e., not negative, forming a triangle).
   * Then, design on your own the `Shape2D` typeclass which is a subclass of `Validable`, define `area` and `circumference` (remove dummy ones). During the design consider using `valid` - for invalid shape, both functions should return zero.
   * Finally, implement instances of `Shape2D` for each shape type.
2. *Data.SortedList* = implement all necessary functions to make data type `SortedList` to be an instance of `Semigroup`, `Monoid`, `Functor`, `Applicative`, `Monad`. After applying `(<>)` the resulting list must be sorted. Sadly, `Functor`, `Applicative`, and `Monad` won't allow you to use `Ord` constaint so the result won't be sorted. Implement some sort function if you want to... It is **forbidden** to just delegate to list and use `fromList` and `toList`.
3. The final (and also the most interesting) task is to implement functions and instances for next data type to represent integers - the `RomanNumeral` (from `Data.RomanNumeral`)! It is a representation of an integer as Roman numeral, for example, `125` is `"CXXV"`.
   * The biggest challenge is to implement functions `integral2RomanNumeral` and `romanNumeral2Integral` (related with `pack` and `unpack`). There is something already prepared in `Data.RomanNumeral.Helpers`. Take a look and feel free to edit as you need.
   * After having `pack` and `unpack` working, it will be very easy to make instances of `Bounded`, `Eq`, `Ord`, `Num`, `Enum`, and `Integral` (*you might need some more due to dependencies*) for the `RomanNumeral`.

Hints & general requirements:

* You should not hardcode any string or character in `Data.RomanNumeral` - use only those defined in `Data.RomanNumeral.Helpers` or enhance it.
* Being [DRY](https://cs.wikipedia.org/wiki/Don%27t_repeat_yourself) is essential, do not repeat code. Name expressions to reuse then, introduce helper functions.
* Local names (via `where` or `let-in`) in functions should be introduced to make the code more readable. Creating very simple helper functions in module scope is not nice. If you need some complicated functions create separate "private" module.
* Make your code as clean as possible. Prefer pattern matching and syntactic sugar everywhere it is reasonable.
* **You must understand your code completely**!

## Notes

  * If you encounter some trouble, create an issue in your repository.
  * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.
  * It might happen that tests are not 100% bulletproof, but your solution should be correct. If you want, propose tests enhancements.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
