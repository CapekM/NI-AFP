module Lib where

import qualified Data.List
import qualified Data.DummyList.Examples
import qualified Data.MyString.Examples

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPES!
data MaritalStatus = Single | Married | Widowed
                   deriving (Show, Read, Eq)

data Gender = Male | Female
            deriving (Show, Read, Eq)

data AcademicTitle = DiS | Bc | Mgr | Ing | PhDr | MUDr | PhD | Doc | Prof
                   deriving (Show, Read, Ord, Eq, Bounded, Enum)

data Person = Person { pFirstname     :: String
                     , pLastname      :: String
                     , pGender        :: Gender
                     , pMaritalStatus :: MaritalStatus
                     , pAge           :: Int
                     , pATitles       :: [AcademicTitle]
                     }

-- | Full czech salutation (in nominative - i.e. první pád)
-- |
-- | "pan doktor Pavel Novák", "paní inženýrka Karolína Šťastná"
-- | "slečna Petra Králová", "Jan" (kid)
-- | if younger than 15 -> kid -> just firstname
-- | if female younger than 25 without academic title and single -> "slečna"
-- | otherwise "pan/paní" depending on the gender
-- | if academic titles, pick the most important (nothing for DiS and Bc)
-- |
-- | https://www.muni.cz/o-univerzite/uredni-deska/oslovovani-akademickych-pracovniku
-- | http://www.etiketavse.estranky.cz/clanky/etiketa/4.-oslovovani-a-spolecenska-vyznamnost.html
-- | http://www.studenta.cz/vysokoskolske-tituly-jak-oslovovat-na-akademicke-pude/magazin/article/587
-- TODO: implement czech salutation which passes the tests
czechSalutation :: Person -> String
czechSalutation p 
    | (pAge p) < 15 = pFirstname p
    | otherwise = (gender_salut p) ++ (title_status p) ++ (name_salut p)

gender_salut :: Person -> String
gender_salut p
    | (pGender p) == Male = "pan "
    | (pAge p) < 25 && (pMaritalStatus p) == Single && (pATitles p) == [] = "slečna "
    | otherwise = "paní "

title_status :: Person -> String
title_status p
    | (pATitles p) == [] = ""
    | otherwise = (titleS p)

titleS :: Person -> String
titleS p 
    | Prof == pMaxTitle = "profesor" ++ (titleGender p "ka")
    | Doc == pMaxTitle = "docent" ++ (titleGender p "ka")
    | elem pMaxTitle [PhD, MUDr, PhDr] = "doktor" ++ (titleGender p "ka")
    | Ing == pMaxTitle = "inženýr" ++ (titleGender p "ka")
    | Mgr == pMaxTitle = "magistr" ++ (titleGender p "a")
    | otherwise = ""
    where pMaxTitle = maximum (pATitles p)

titleGender :: Person -> String -> String
titleGender p s
    | (pGender p) == Male = " "
    | otherwise = s ++ " "

name_salut :: Person -> String
name_salut p = (pFirstname p) ++ " " ++ (pLastname p)

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
-- https://en.wikipedia.org/wiki/Interval_(mathematics)
data IntervalBoundary = PositiveInfinity
                      | NegativeInfinity
                      | Inclusive Double
                      | Exclusive Double
                      deriving (Show, Read, Eq)

data Interval = Empty
              | Interval IntervalBoundary IntervalBoundary
              | Union [Interval]
              | Disjoint [Interval]
              | AllNumbers
              deriving (Show, Read, Eq)

-- | Check if number is in given interval
intervalContains :: Interval -> Double -> Bool
intervalContains Empty _ = False
intervalContains (Interval l u) x = greater_than l x && smaller_than u x
intervalContains (Union i) x = or $ map helper i
    where helper ii = intervalContains ii x
intervalContains (Disjoint i) x = and $ map helper i
    where helper ii = intervalContains ii x
intervalContains AllNumbers _ = True

smaller_than :: IntervalBoundary -> Double -> Bool
smaller_than PositiveInfinity _ = True
smaller_than NegativeInfinity _ = False
smaller_than (Inclusive i) x = i >= x
smaller_than (Exclusive e) x = e > x

greater_than :: IntervalBoundary -> Double -> Bool
greater_than PositiveInfinity _ = False
greater_than NegativeInfinity _ = True
greater_than (Inclusive i) x = i <= x
greater_than (Exclusive e) x = e < x

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
data Shape2D = Circle { ciRadius :: Double }
             | Square { sqSide :: Double }
             | Rectangle { reWidth :: Double, reHeight :: Double }
             | Triangle { triSideA :: Double, triSideB :: Double, triSideC :: Double }
             deriving (Show, Read, Eq)

-- TODO: implement circumference calculation for 2D shapes
shapeCircumference :: Shape2D -> Double
shapeCircumference (Circle r) = 2 * pi * r
shapeCircumference (Square x) = 4 * x
shapeCircumference (Rectangle x y) = 2 * x + 2 * y
shapeCircumference (Triangle a b c) = a + b + c

-- TODO: implement area calculation for 2D shapes
shapeArea :: Shape2D -> Double
shapeArea (Circle r) = pi * r * r
shapeArea (Square x) = x * x
shapeArea (Rectangle x y) = x * y
shapeArea triangle@(Triangle a b c) = sqrt $ s * (s - a) * (s - b) * (s - c)
    where s = (shapeCircumference triangle) / 2

-------------------------------------------------------------------------------
-- | Arithmetic sequence as infinite list
-- | https://en.wikipedia.org/wiki/Arithmetic_progression
-- TODO: implement arithmetic series
arithmeticSequence :: Num b => b -> b -> [b]
arithmeticSequence a d = iterate (+d) a


-- TODO: implement infinite list of fibonacciNumbers [0, 1, 1, 2, 3, 5, ...]
fibonacciNumbers :: [Integer]
fibonacciNumbers = 0 : 1 : zipWith (+) fibonacciNumbers (tail fibonacciNumbers)

-- also works
-- fibonacciNumbers = 0 : next
--     where next = 1 : zipWith (+) fibonacciNumbers nxt

-- TODO: multiply matrices x and y
-- TODO: use list comprehension!!!
-- https://en.wikipedia.org/wiki/Matrix_multiplication
-- Note: sublists are rows
-- if wrong sizes, raise error "Incorrect matrix sizes" (use "error" function)
matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication a b
    | (length b) == (length $ head a) = make_matrix [my_product x  y | x <- a , y <- (Data.List.transpose b) ] (length $ head b)
    | otherwise = error "Incorrect matrix sizes"

my_product :: Num a => [a] -> [a] -> a
my_product [] [] = 0
my_product [] _ = error "Incorrect matrix sizes"
my_product _ [] = error "Incorrect matrix sizes"
my_product (x:xs) (y:ys) = (x * y) + (my_product xs ys)

make_matrix :: Num a => [a] -> Int -> [[a]]
make_matrix [] _ = []
make_matrix l n = (take n l) : (make_matrix (drop n l) n)

-------------------------------------------------------------------------------
-- !!! DO NOT COPY, JUST IMPORT (avoid conflicts, pick the best option for you)
-- iii visit the content of modules
-- TODO: replace undefined with "example1" from Data.DummyList.Examples module
dummyListExample1 = Data.DummyList.Examples.example1
-- TODO: replace undefined with "example2" from Data.MyString.Examples module
stringExample2 = Data.MyString.Examples.example2
-- TODO: replace undefined with "example3" from Data.MyString.Examples module
stringExample3 = Data.MyString.Examples.example3

