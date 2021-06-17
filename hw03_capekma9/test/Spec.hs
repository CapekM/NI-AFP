import Test.Hspec

import Control.Exception

import Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

matrixA = [[1,2],[3,4]]
matrixB = [[5,5],[1,7]]
matrixC = [[3,1,2],[7,2,1]]
matrixD = [[3,1],[2,1],[1,3]]
matrixE = [[1,2,3,4]]

eye10 = [
            [1,0,0,0,0,0,0,0,0,0],
            [0,1,0,0,0,0,0,0,0,0],
            [0,0,1,0,0,0,0,0,0,0],
            [0,0,0,1,0,0,0,0,0,0],
            [0,0,0,0,1,0,0,0,0,0],
            [0,0,0,0,0,1,0,0,0,0],
            [0,0,0,0,0,0,1,0,0,0],
            [0,0,0,0,0,0,0,1,0,0],
            [0,0,0,0,0,0,0,0,1,0],
            [0,0,0,0,0,0,0,0,0,1]
        ]

spec :: Spec
spec = do
  describe "czechSalutation" $ do
    it "salutates to kids just with firstname" $ do
      czechSalutation (Person "Tom" "Sawyer" Male Single 12 []) `shouldBe` "Tom"
      czechSalutation (Person "Alice" "Wonderland" Female Single 7 []) `shouldBe` "Alice"
    it "salutates men without titles" $ do
      czechSalutation (Person "Prokop" "Buben" Male Single 15 []) `shouldBe` "pan Prokop Buben"
      czechSalutation (Person "Aleš" "Novák" Male Married 20 []) `shouldBe` "pan Aleš Novák"
      czechSalutation (Person "Jan" "Nerdský" Male Single 35 []) `shouldBe` "pan Jan Nerdský"
      czechSalutation (Person "Alfons" "Příbramský" Male Widowed 100 []) `shouldBe` "pan Alfons Příbramský"
    it "salutates women without titles" $ do
      czechSalutation (Person "Petra" "Novotná" Female Single 17 []) `shouldBe` "slečna Petra Novotná"
      czechSalutation (Person "Lucie" "Smutná" Female Widowed 19 []) `shouldBe` "paní Lucie Smutná"
      czechSalutation (Person "Emma" "Mléko" Female Married 20 []) `shouldBe` "paní Emma Mléko"
      czechSalutation (Person "Dita" "Zelená" Female Single 35 []) `shouldBe` "paní Dita Zelená"
      czechSalutation (Person "Aloisie" "Příbramská" Female Widowed 100 []) `shouldBe` "paní Aloisie Příbramská"
    it "salutates men with titles" $ do
      czechSalutation (Person "Prokop" "Buben" Male Single 20 [Mgr]) `shouldBe` "pan magistr Prokop Buben"
      czechSalutation (Person "Sheldon" "Cooper" Male Single 30 [PhD]) `shouldBe` "pan doktor Sheldon Cooper"
      czechSalutation (Person "Ivo" "Zlý" Male Widowed 35 [DiS, Bc]) `shouldBe` "pan Ivo Zlý"
      czechSalutation (Person "Tomáš" "Marný" Male Married 35 [DiS, Bc, Ing, PhD]) `shouldBe` "pan doktor Tomáš Marný"
      czechSalutation (Person "Josef" "Šteng" Male Single 38 [Mgr, PhDr]) `shouldBe` "pan doktor Josef Šteng"
      czechSalutation (Person "Bruno" "Marek" Male Single 55 [MUDr]) `shouldBe` "pan doktor Bruno Marek"
      czechSalutation (Person "Mojmír" "Čepek" Male Married 69 [Mgr, Ing, Doc]) `shouldBe` "pan docent Mojmír Čepek"
      czechSalutation (Person "Miroslav" "Král" Male Widowed 83 [Ing, Prof]) `shouldBe` "pan profesor Miroslav Král"
    it "salutates women with titles" $ do
      czechSalutation (Person "Klára" "Jílková" Female Single 20 [Mgr]) `shouldBe` "paní magistra Klára Jílková"
      czechSalutation (Person "Amy Farrah" "Fowler" Female Single 30 [PhD]) `shouldBe` "paní doktorka Amy Farrah Fowler"
      czechSalutation (Person "Andrea" "Zlá" Female Widowed 35 [DiS, Bc]) `shouldBe` "paní Andrea Zlá"
      czechSalutation (Person "Zdislava" "Zděšená" Female Married 35 [DiS, Bc, Ing, PhD]) `shouldBe` "paní doktorka Zdislava Zděšená"
      czechSalutation (Person "Zuzana" "Zimová" Female Single 42 [Mgr, PhDr]) `shouldBe` "paní doktorka Zuzana Zimová"
      czechSalutation (Person "Marie" "Rusová" Female Single 57 [MUDr]) `shouldBe` "paní doktorka Marie Rusová"
      czechSalutation (Person "Anastazie" "Rudá" Female Married 66 [Mgr, Ing, Doc]) `shouldBe` "paní docentka Anastazie Rudá"
      czechSalutation (Person "Denisa" "Šídlová" Female Widowed 74 [Ing, PhD, Prof]) `shouldBe` "paní profesorka Denisa Šídlová"

  describe "intervalContains" $ do
    it "works for trivial intervals" $ do
      intervalContains Empty 5 `shouldBe` False
      intervalContains AllNumbers 5 `shouldBe` True
      intervalContains (Interval NegativeInfinity PositiveInfinity) 5 `shouldBe` True
      intervalContains (Interval (Inclusive 4) PositiveInfinity) 5 `shouldBe` True
      intervalContains (Interval NegativeInfinity (Exclusive 10)) 5 `shouldBe` True
      intervalContains (Interval (Exclusive 5) (Inclusive 10)) 7 `shouldBe` True
      intervalContains (Interval (Exclusive 5) (Inclusive 10)) 15 `shouldBe` False
      intervalContains (Interval (Exclusive 5) (Inclusive 10)) 3 `shouldBe` False
      intervalContains (Interval (Inclusive 5) (Inclusive 5)) 5 `shouldBe` True
    it "works for incorrect intervals" $ do
      intervalContains (Interval PositiveInfinity NegativeInfinity) 5 `shouldBe` False
      intervalContains (Interval PositiveInfinity (Inclusive 5)) 5 `shouldBe` False
      intervalContains (Interval (Inclusive 5) NegativeInfinity) 5 `shouldBe` False
      intervalContains (Interval (Inclusive 6) (Inclusive 4)) 5 `shouldBe` False
    it "works for inclusive and exclusive bounds" $ do
      intervalContains (Interval (Exclusive 5) (Inclusive 10)) 10 `shouldBe` True
      intervalContains (Interval (Exclusive 5) (Inclusive 10)) 10.0001 `shouldBe` False
      intervalContains (Interval (Exclusive 5) (Inclusive 10)) 5 `shouldBe` False
      intervalContains (Interval (Exclusive 5) (Inclusive 10)) 5.0001 `shouldBe` True
      intervalContains (Interval (Inclusive 2) (Exclusive 7)) 7 `shouldBe`  False
      intervalContains (Interval (Inclusive 0) (Exclusive 1)) 0 `shouldBe` True
    it "works for infinite bounds" $ do
      intervalContains (Interval (Exclusive 0) PositiveInfinity) 452 `shouldBe` True
      intervalContains (Interval (Exclusive 0) PositiveInfinity) (-2) `shouldBe` False
      intervalContains (Interval NegativeInfinity (Exclusive 0)) (-507) `shouldBe` True
      intervalContains (Interval NegativeInfinity (Exclusive 0)) 0.75 `shouldBe` False
    it "works for unions" $ do
      intervalContains (Union [Interval (Inclusive 5) (Inclusive 7), Interval (Exclusive 15) (Exclusive 20)]) 4 `shouldBe` False
      intervalContains (Union [Interval (Inclusive 5) (Inclusive 7), Interval (Exclusive 15) (Exclusive 20)]) 6 `shouldBe` True
      intervalContains (Union [Interval (Inclusive 5) (Inclusive 7), Interval (Exclusive 15) (Exclusive 20)]) 8 `shouldBe` False
      intervalContains (Union [Interval (Inclusive 5) (Inclusive 7), Interval (Exclusive 15) (Exclusive 20)]) 16 `shouldBe` True
      intervalContains (Union [Interval (Inclusive 5) (Inclusive 7), Interval (Exclusive 15) (Exclusive 20)]) 20 `shouldBe` False
    it "works for disjoints" $ do
      intervalContains (Disjoint [Interval (Inclusive 5) (Inclusive 10), Interval (Exclusive 6) (Exclusive 20)]) 6 `shouldBe` False
      intervalContains (Disjoint [Interval (Inclusive 5) (Inclusive 10), Interval (Exclusive 6) (Exclusive 20)]) 6.66 `shouldBe` True
      intervalContains (Disjoint [Interval (Inclusive 5) (Inclusive 10), Interval (Exclusive 6) (Exclusive 20)]) 10 `shouldBe` True
      intervalContains (Disjoint [Interval (Inclusive 5) (Inclusive 10), Interval (Exclusive 6) (Exclusive 20)]) 10.1 `shouldBe` False
    it "works for complex intervals" $ do
      intervalContains (Disjoint [Union [Interval (Exclusive 0) (Exclusive 5), Interval (Inclusive 5) (Exclusive 7), Interval (Inclusive 10) (Exclusive 20)], Interval (Exclusive 6) (Exclusive 15)]) 6.5 `shouldBe` True
      intervalContains (Disjoint [Union [Interval (Exclusive 0) (Exclusive 5), Interval (Inclusive 5) (Exclusive 7), Interval (Inclusive 10) (Exclusive 20)], Interval (Exclusive 6) (Exclusive 15)]) 11 `shouldBe` True
      intervalContains (Disjoint [Union [Interval (Exclusive 0) (Exclusive 5), Interval (Inclusive 5) (Exclusive 7), Interval (Inclusive 10) (Exclusive 20)], Interval (Exclusive 6) (Exclusive 15)]) 8 `shouldBe` False
      intervalContains (Union [Disjoint [Interval (Exclusive 3) (Exclusive 6), Interval (Inclusive 4) (Inclusive 8)], Interval (Inclusive 5) (Exclusive 15)]) 7 `shouldBe` True
      intervalContains (Union [Disjoint [Interval (Exclusive 3) (Exclusive 6), Interval (Inclusive 4) (Inclusive 8)], Interval (Inclusive 5) (Exclusive 15)]) 4 `shouldBe` True
      intervalContains (Union [Disjoint [Interval (Exclusive 3) (Exclusive 6), Interval (Inclusive 4) (Inclusive 8)], Interval (Inclusive 5) (Exclusive 15)]) 3 `shouldBe` False
      intervalContains (Union [Disjoint [Interval (Exclusive 3) (Exclusive 6), Interval (Inclusive 4) (Inclusive 8)], Interval (Inclusive 5) (Exclusive 15)]) 15 `shouldBe` False

  describe "shapeCircumference" $ do
    it "computes circumference of circle" $ do
      shapeCircumference (Circle 5) `shouldBe` 10.0 * pi
      shapeCircumference (Circle 1) `shouldBe` 2.0 * pi
      shapeCircumference (Circle 10) `shouldBe` 20.0 * pi
    it "computes circumference of square" $ do
      shapeCircumference (Square 5)  `shouldBe` 20
      shapeCircumference (Square 1) `shouldBe` 4
      shapeCircumference (Square 10) `shouldBe` 40
    it "computes circumference of rectangle" $ do
      shapeCircumference (Rectangle 2 5) `shouldBe` 14
      shapeCircumference (Rectangle 5 5) `shouldBe` 20
      shapeCircumference (Rectangle 3 1) `shouldBe` 8
    it "computes circumference of triangle" $ do
      shapeCircumference (Triangle 2 5 4) `shouldBe` 11
      shapeCircumference (Triangle 5 5 5) `shouldBe` 15
      shapeCircumference (Triangle 3 2 4) `shouldBe` 9

  describe "shapeArea" $ do
    it "computes area of circle" $ do
      shapeArea (Circle 5) `shouldBe` 25 * pi
      shapeArea (Circle 1) `shouldBe` pi
      shapeArea (Circle 10) `shouldBe` 100 * pi
    it "computes area of square" $ do
      shapeArea (Square 5) `shouldBe` 25
      shapeArea (Square 1) `shouldBe` 1
      shapeArea (Square 10) `shouldBe` 100
    it "computes circumference of rectangle" $ do
      shapeArea (Rectangle 2 5) `shouldBe` 10
      shapeArea (Rectangle 5 5) `shouldBe` 25
      shapeArea (Rectangle 3 1) `shouldBe` 3
    it "computes area of triangle" $ do
      shapeArea (Triangle 3 5 4) `shouldBe` 6
      shapeArea (Triangle 5 5 5) `shouldBe` sqrt 117.1875
      shapeArea (Triangle 3 2 4) `shouldBe` sqrt 8.4375

  describe "arithmeticSequence" $ do
    it "has head always equal to a" $ do
      head (arithmeticSequence 5 0) `shouldBe` 5
      head (arithmeticSequence 5 10) `shouldBe` 5
      head (arithmeticSequence 5 0.5) `shouldBe` 5
      head (arithmeticSequence 5 (-2)) `shouldBe` 5
    it "works with d=0 as repeat" $ do
      take 20 (arithmeticSequence 5 0) `shouldBe` replicate 20 5
      take 20 (arithmeticSequence (-5) 0) `shouldBe` replicate 20 (-5)
      take 20 (arithmeticSequence 0 0) `shouldBe` replicate 20 0
    it "works with integers d>0" $ do
      take 10 (arithmeticSequence 2 2) `shouldBe` [2,4,6,8,10,12,14,16,18,20]
      take 10 (arithmeticSequence 5 5) `shouldBe` [5,10,15,20,25,30,35,40,45,50]
      take 10 (arithmeticSequence 2 3) `shouldBe` [2,5,8,11,14,17,20,23,26,29]
    it "work with integers d<0" $ do
      take 10 (arithmeticSequence 2 (-1)) `shouldBe` [2,1,0,-1,-2,-3,-4,-5,-6,-7]
      take 10 (arithmeticSequence (-1) (-2)) `shouldBe` [-1,-3,-5,-7,-9,-11,-13,-15,-17,-19]
      take 10 (arithmeticSequence 0 (-5)) `shouldBe` [0,-5,-10,-15,-20,-25,-30,-35,-40,-45]
    it "seems to be infinite" $ do
      arithmeticSequence 0 2 `shouldContain` [1024]
      arithmeticSequence (-257) 19 `shouldContain` [(-257) + 1900]
      arithmeticSequence 5 (-5) `shouldContain` [(-5)*21786]
  describe "fibonacciNumbers" $ do
    it "contains basic fibonacci numbers" $ do
      fibonacciNumbers `shouldContain` [3]
      fibonacciNumbers `shouldContain` [5]
      fibonacciNumbers `shouldContain` [8]
      fibonacciNumbers `shouldContain` [13]
      fibonacciNumbers `shouldContain` [21]
    it "contains various big fibonacci numbers" $ do
      fibonacciNumbers `shouldContain` [6765]
      fibonacciNumbers `shouldContain` [75025]
      fibonacciNumbers `shouldContain` [514229]
      fibonacciNumbers `shouldContain` [832040]
      fibonacciNumbers `shouldContain` [354224848179261915075]
    it "does not contain non-fibbonacci numbers" $ do
      let fibonacciNumbers3000 = take 3000 fibonacciNumbers
      fibonacciNumbers3000 `shouldNotContain` [4]
      fibonacciNumbers3000 `shouldNotContain` [27]
      fibonacciNumbers3000 `shouldNotContain` [99]
      fibonacciNumbers3000 `shouldNotContain` [666]
      fibonacciNumbers3000 `shouldNotContain` [2942]
    it "does not contain negative numbers" $ do
      let fibonacciNumbers10000 = take 10000 fibonacciNumbers
      all (>=0) fibonacciNumbers10000 `shouldBe` True
    it "has primes on correct positions" $ do
      head fibonacciNumbers `shouldBe` 0
      fibonacciNumbers !! 1 `shouldBe` 1
      fibonacciNumbers !! 15 `shouldBe` 610
      fibonacciNumbers !! 42 `shouldBe` 267914296
      fibonacciNumbers !! 666 `shouldBe` 6859356963880484413875401302176431788073214234535725264860437720157972142108894511264898366145528622543082646626140527097739556699078708088

  describe "matrixMultiplication" $ do
    it "works for trivial cases" $ do
      matrixMultiplication [[1]] [[1]] `shouldBe` [[1]]
      matrixMultiplication [[1]] [[2]] `shouldBe` [[2]]
      matrixMultiplication [[10]] [[0]] `shouldBe` [[0]]
    it "works for identity matrix" $ do
      matrixMultiplication matrixA [[1,0],[0,1]] `shouldBe` matrixA
      matrixMultiplication matrixB [[1,0],[0,1]] `shouldBe` matrixB
      matrixMultiplication [[1,0],[0,1]] matrixA `shouldBe` matrixA
      matrixMultiplication [[1,0],[0,1]] matrixB `shouldBe` matrixB
    it "works with square matrices" $ do
      matrixMultiplication matrixA matrixB `shouldBe` [[7,19],[19,43]]
      matrixMultiplication matrixB matrixA `shouldBe` [[20,30],[22,30]]
    it "works with correct sizes" $ do
      matrixMultiplication matrixA matrixC `shouldBe` [[17,5,4],[37,11,10]]
      matrixMultiplication matrixB matrixC `shouldBe` [[50,15,15],[52,15,9]]
      matrixMultiplication matrixC matrixD `shouldBe` [[13,10],[26,12]]
    it "raises error for bad sizes" $ do
      evaluate (matrixMultiplication [[1]] matrixA) `shouldThrow` errorCall "Incorrect matrix sizes"
      evaluate (matrixMultiplication matrixA eye10) `shouldThrow` errorCall "Incorrect matrix sizes"
      evaluate (matrixMultiplication matrixE matrixD) `shouldThrow` errorCall "Incorrect matrix sizes"

  describe "imports" $ do
    it "has correct example 1" $
      take 100 dummyListExample1 `shouldBe` replicate 100 25
    it "has correct example 2" $
      stringExample2 `shouldBe` "Example 2"
    it "has correct example 3" $
      stringExample3 `shouldBe` "Example 3"
