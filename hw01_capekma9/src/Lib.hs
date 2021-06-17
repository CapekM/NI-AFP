module Lib
    ( convexRegularPolygonArea
    , leapYear
    , infoBackwards
    , countDigits
    , manhattanDistance
    , hammingDistance
    , stringToLines
    , filterType
    , bitvecAuthor
    , pluralizeFunc
    ) where

import Language.English.Plural

-- #01 = Area of convex regular polygon (check tests if not sure; use Pi from Prelude)
convexRegularPolygonArea n s = ( n * ( s ^ 2 ) ) / ( 4 * tan ( pi / n ) )

-- #02 = Write boolean expression (avoid if-then-else) to check if year is leap
leapYear :: Word -> Bool
leapYear year = ( mod year 4 == 0 ) && ( mod year 100 /= 0 ) || ( mod year 400 == 0 )

-- #03 = Message about reversed string ("'abc' is 'cba' backwards")
infoBackwards :: String -> String
infoBackwards str = "'" ++ ( reverse str ) ++ "' is '" ++ str ++ "' backwards"

-- #04 = Count number of digits of given integer
-- (It can be done just by looking up suitable functions.
--  Try it WITHOUT conditions and recursion!)
countDigits x = length ( show ( abs x ) )

-- #05 = Return Manhattan distance of 2 2D points
-- (use variables x1, y1, x2, y2 in expression instead of undefined)
manhattanDistance (x1, y1) (x2, y2) = abs ( x1 - x2 ) + abs ( y1 - y2 )

-- #06 = Complete the function to compute Hamming distance of two
-- strings such as "1001" and "0111" (result should be 3). But the
-- strings not necessarily contain only 0s and 1s.
--
-- String is just a list of char ([Char])... Recursion for the win!
-- Pattern matching is useful... define cases with empty lists and
-- then use the one where x and y are heads of the lists, and xs and
-- ys are tails.
hammingDistance :: Num a => String -> String -> a
hammingDistance [] [] = 0
hammingDistance []  _ = 0
hammingDistance  _ [] = 0
hammingDistance (x:xs) (y:ys)
    | x /= y = 1 + tl
    | otherwise = tl
    where tl = hammingDistance xs ys

-- #07 = Lookup a function that splits String by newlines
stringToLines :: String -> [String]
stringToLines = lines

-- #08 = What is the type of function "filter"?
-- (For example for "odd" it would be "Integral a => a -> Bool")
filterType :: String
filterType = "(a -> Bool) -> [a] -> [a]"

-- #09 = Who is author of "bitvec" package? (the most recent one)
-- (For example for "QuickCheck" it would be "Koen Claessen")
bitvecAuthor :: String
bitvecAuthor = "Andrew Lelechenko"

-- #10 = You need a function that returns pluralized form of
--       English string ("letter" -> "letters", "tooth -> teeth", ...)
-- (Hint: do not reinvent the wheel, look it up and use as dependency!)
pluralizeFunc :: String -> String
pluralizeFunc = plural
